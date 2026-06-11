     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H****************************************************************
     H/TITLE SC736R - DARTS DIARY LISTING - WORK FILE PGM
     H****************************************************************
     H**   PROGRAM DESCRIPTION: BUILD WORKFILE WXDDP FOR REPORT PGM. **
     H**
     H**
     H**
     H**   CALLED FROM:                                              **
     H**          SC736C                                             **
     H**   CALLS TO:                                                 **
     H**          ------                                             **
     H**   PROGRAMMER: T. Anzalone                                   **
     H**   CREATED:    09/05/91     PROJECT: AIT                     **
     H****************************************************************
     H** REVISIONS:                                                  **
     H*
     H*  05/25/07 C:191 PROJECT: WC1515
     H*                 Check claims authority
     H*
     H*  06/22/05 C:58  SUPPORT: WC1312 41194
     H*                 SET AND READ SPECIFIC USER IF SPECIFIED
     H*
     H*  06/21/05 C:136 SUPPORT: DR135 47398                          *
     H*                 CLAIMENT NAME IS PRINTING WHERE THE KEY IS    *
     H*                 SUPPOSED TO BE PRINTED. CHG CALL TO 'SRKCVT'  *
     H*                 TO 'SRKCVT1' SO THAT KEY WILL BE RETURNED     *
     H*                 INSTEAD OF NAME.                              *
     H*                                                               *
     H** 04/27/92 KRR   PROJECT: AJP                                 **
     H**                CHANGE REFERENCE FIELDS FROM CMCODE(2A) TO   **
     H**                CMRMCD(5A).                                  **
     H** 04/13/94 C04   PROJECT: DR004                               **
     H**                PER TLB: INCLUDE REMARKS ADDED FROM U0010R,  **
     H**                F0075R, U0041R, N0160R.  THIS IS SORT OF A   **
     H**                TEMPORARY FIX TO GET CLIENT 50 RUNNING.  IF  **
     H**                THIS IS INTENDED TO BE BASE, WE WILL NEED TO **
     H**                CONSIDER SYSTEM FLAGS TO DETERMINE WHAT KEY  **
     H**                IS TO BE USED.                               **
     H****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**      OF      OVERFLOW FOR PRINTING                          **
     H**                                                             **
     H**      08      Batch Mode                                     **
     H**      09      Demand Mode                                    **
     H**      60      GENERAL CHAIN                                  **
     H**      70      GENERAL CAS                                    **
     H*****************************************************************
     H** FILE USAGE:
     H** FILES:                                                      **
     H**    FMUPL (PUP01): Q1USER                                    **
     H**    WMCDL (PCD01): SS#                                       **
     H**    WMCML1(PCM02): CO/FUND/FYR/CASE                          **
     H**    SDCML9(PCM01): USER/PNAM/KEY/CDAT (S) DIARY FLAG = Y/    **
     H**                                          CMSTAT NE I        **
     H*****************************************************************
     F*
     FWMUPL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FWMCDL     IF   E           K DISK
     FWXDDP     O    E           K DISK
     FSDCML9    IF   E           K DISK

     D*  SYSTEM DATE AND  TIME DATA STRUCTURE
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D                 DS
     D  QQKEY                  1     40
     D  Q1CO#                  1      3  0
     D  Q1FND                  4      6  0
     D  Q1FYR                  7      9  0
     D  Q1CASE                10     16  0
     D****************************************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     IPCM02
     I              CMCO#                       CM2CO#
     I              CMUSER                      CM2SER

     C*
     C*  INITIALIZE WORK FIELDS
     C     *LIKE         DEFINE    CM2CO#        XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMCDAT        XXCDAT
     C     *LIKE         DEFINE    CDLSN         XXLSN
     C     *LIKE         DEFINE    CDFSN         XXFSN
     C     *LIKE         DEFINE    CDMID         XXMID
     C     *LIKE         DEFINE    CMPNAM        XXPNAM
     C     *LIKE         DEFINE    CMUSER        XXUSER
     C*
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*
     C*   KEY LIST FOR THE SECURITY FILE
     C*
     C     SLKY          KLIST
     C                   KFLD                    XXDUSR           10
     C*
     C*** BEG ADD ***** 06/22/05 **********************************
     C*  DARTS FILE
     C     DRKEY         KLIST
     C                   KFLD                    QQDUSR
     C*
     C*** END ADD ***** 06/22/05 **********************************
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
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDUSR           10
     C                   PARM                    QQCODE            5
     C                   PARM                    QQSTAT            1
     C                   PARM                    QQCDAT            8
     C                   PARM                    QQSORT            1
     C                   PARM                    QQREC             1
     C*
     C                   MOVE      QQREC         XXREC             1
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C                   TIME                    DSDTTM
     C*
     C*
     C*
     C*   START READING THE COMMENT DETAIL FILE
     C*** BEG CHG ***** 06/22/05 **********************************
     C     QQDUSR        IFNE      *BLANK
     C     DRKEY         CHAIN     PCM01                              60
     C                   ELSE                                                   X.DUSR <> ' '
     C                   READ      PCM01                                  60
     C                   ENDIF                                                  e.DUSR <> ' '
     C*
     C****************     READ PCM01                    60
     C*** END CHG ***** 06/22/05 **********************************
     C*
     C*    Read all.
     C     *IN60         DOWEQ     '0'
     C*
     C     XXREC         IFEQ      'C'
     C     CMPNAM        ANDEQ     'F0070R'
     C     XXREC         OREQ      'C'
     C     CMPNAM        ANDEQ     'W0070R'
     C     XXREC         OREQ      'C'
     C     CMPNAM        ANDEQ     'U0010R'
     C     XXREC         OREQ      'C'
     C     CMPNAM        ANDEQ     'U0041R'
     C     XXREC         OREQ      'C'
     C     CMPNAM        ANDEQ     'F0075R'
     C     XXREC         OREQ      'C'
     C     CMPNAM        ANDEQ     'N0160R'
     C     XXREC         OREQ      'E'
     C     CMPNAM        ANDEQ     'W0050R'
     C     XXREC         OREQ      'D'
     C     CMPNAM        ANDEQ     'W0050RD'
     C     XXREC         OREQ      'A'
     C     CMPNAM        ANDEQ     'W0050RA'
     C     XXREC         OREQ      'P'
     C     CMPNAM        ANDEQ     'W0030R'
     C     XXREC         OREQ      'P'
     C     CMPNAM        ANDEQ     'F0031R'
     C     XXREC         OREQ      'L'
     C     CMPNAM        ANDEQ     'W0040R'
     C*
     C                   EXSR      BUILDK
     C*
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C     XXREC         IFEQ      'C'
     C                   MOVE      *BLANK        QQKEY
     C                   MOVEL     CMKEY         QQKEY
     C                   MOVE      Q1CO#         ##CO#
     C                   MOVE      Q1FND         ##FND
     C                   MOVE      Q1FYR         ##FYR
     C                   MOVE      Q1CASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   GOTO      JUMP
     C                   ENDIF                                                     RTCD = '2'
     C                   ENDIF                                                     XXREC= 'C'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C     XXREC         CASEQ     'C'           SRCASE                   70
     C                   END
     C     XXREC         CASEQ     'E'           SREMP                    70
     C                   END
     C     XXREC         CASEQ     'D'           SREMPD                   70
     C                   END
     C     XXREC         CASEQ     'A'           SRAGT                    70
     C                   END
     C     XXREC         CASEQ     'P'           SRPROV                   70
     C                   END
     C     XXREC         CASEQ     'L'           SRLOSS                   70
     C                   END
     C*
     C*   CLEAR THE FIELDS IN THE WORK FILE
     C*
     C*
     C                   MOVE      *BLANK        DDKEY
     C                   MOVE      *BLANK        DD#KY
     C                   MOVE      *BLANK        DDCDAT
     C                   MOVE      *BLANK        DDCOMM
     C                   MOVE      *BLANK        DDSS#
     C                   MOVE      *BLANK        DDLSN
     C                   MOVE      *BLANK        DDFSN
     C                   MOVE      *BLANK        DDMID
     C                   MOVE      *BLANK        DDCNAM
     C                   MOVE      *BLANK        DDACDT
     C                   MOVE      *BLANK        DDCODE
     C                   MOVE      *BLANKS       DDRMCD
     C                   MOVE      *BLANK        DDSTAT
     C                   MOVE      *BLANK        DDUSER
     C                   MOVE      *BLANK        DDDCDT
     C                   MOVE      *ZEROS        DDSEQ#
     C*
     C*   MOVE THE INFORMATION TO CORRECT FIELDS
     C*
     C                   MOVE      CMKEY         DDKEY
     C                   MOVE      X#KEY         DD#KY
     C                   MOVE      CMCDAT        DDCDAT
     C                   MOVE      CMCOMM        DDCOMM
     C*
     C                   MOVE      CMSS#         DDSS#
     C                   MOVE      CDLSN         DDLSN
     C                   MOVE      CDFSN         DDFSN
     C                   MOVE      CDMID         DDMID
     C                   MOVE      XXCNAM        DDCNAM
     C*
     C                   MOVE      CMACDT        DDACDT
     C                   MOVE      CMCODE        DDCODE
     C                   MOVE      CMRMCD        DDRMCD
     C*
     C                   MOVE      CMSTAT        DDSTAT
     C                   MOVE      CMUSER        DDUSER
     C                   MOVE      CMDCDT        DDDCDT
     C                   MOVE      CMSEQ#        DDSEQ#
     C*
     C                   WRITE     PDD01
     C*
     C     JUMP          TAG
     C                   END                                                    ENDIFXXREC
     C*** BEG CHG ***** 06/22/05 **********************************
     C     QQDUSR        IFNE      *BLANK
     C     DRKEY         READE     PCM01                                  60
     C                   ELSE                                                   X.DUSR <> ' '
     C                   READ      PCM01                                  60
     C                   ENDIF                                                  e.DUSR <> ' '
     C                   ENDDO                                                  DOWEQ*IN60
     C*
     C****************     READ PCM01                    60
     C****************     END                             DOWEQ*IN60
     C*** END CHG ***** 06/22/05 **********************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*================================================================
     C*   BUILDK    -  BUILD KEY DISPLAY SUBROUTINE                   *
     C*================================================================
     C*
     C* BEGIN SUBROUTINE
     C*
     C     BUILDK        BEGSR
     C                   MOVEL(P)  CMKEY         X#KEY
     C                   MOVE      ' '           X#RTCD
     C                   MOVEL(P)  CMPNAM        X#PNAM
      *** BEG CHG ***** 06/21/05 **********************************
      *****************
     C*****************    CALL 'SRKCVT'
     C*****************    PARM           X#PNAM 10
     C*****************    PARM           X#KEY  79
     C*****************    PARM           X#RTCD  1
     C                   CALL      'SRKCVT1'
     C                   PARM                    X#PNAM           10
     C                   PARM                    X#KEY            79
     C                   PARM      *BLANKS       X#NAME           79
     C                   PARM                    X#RTCD            1
      *** END CHG ***** 06/21/05 **********************************
     C*
     C     X#RTCD        IFNE      'N'
     C                   MOVEL     X#KEY         XXKEY            79
     C                   ELSE
     C                   MOVEL     CMKEY         XXKEY
     C                   END
     C*  END OF SUBROUTINE
     C                   ENDSR
     C*
      *================================================================
      *   SRCASE    -  SUBROUTINE                                     *
      *================================================================
     C*
     C* BEGIN SUBROUTINE
     C*
     C     SRCASE        BEGSR
     C*
     C* CHAIN TO CLAIM MASTER TO GET SS#, ACCIDENT DATE
     C                   MOVE      *BLANK        QQKEY
     C                   MOVEL     CMKEY         QQKEY
     C                   Z-ADD     0             XXCO#
     C                   Z-ADD     0             XXFND
     C                   Z-ADD     0             XXFYR
     C                   Z-ADD     0             XXCASE
     C                   MOVE      Q1CO#         XXCO#
     C                   MOVE      Q1FND         XXFND
     C                   MOVE      Q1FYR         XXFYR
     C                   MOVE      Q1CASE        XXCASE
     C     CKEY          CHAIN     PCM02                              60
     C                   MOVE      *BLANK        XXSS#
     C                   MOVE      CMSS#         XXSS#
     C*
     C*  CHAIN TO CLAIMANT DEMOGRAPHICS TO GET CLAIMANT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C*
     C*  GENRATE LAST NAME, FIRST AND MIDDLE INITIAL
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       QQFMN            15
     C                   MOVE      *BLANKS       QQFSN            15
     C                   MOVE      *BLANKS       QQMID            56
     C                   MOVE      '1'           QQTERM            1
     C                   MOVEL     CDFSN         QQFSN
     C                   MOVEL     CDMID         QQMID
     C                   CALL      'SRFRLS'
     C                   PARM                    QQMID
     C                   PARM                    QQFSN
     C                   PARM                    QQTERM
     C                   MOVEL     QQMID         QQFMN
     C*
     C                   MOVE      '1'           QQTERM
     C                   MOVE      *BLANKS       QQLSN            56
     C                   MOVEL     CDLSN         QQLSN
     C                   CALL      'SRNAME'
     C                   PARM                    QQLSN
     C                   PARM                    QQFMN
     C                   PARM                    QQTERM
     C                   MOVEL     QQLSN         XXCNAM           30
     C                   ELSE
     C                   MOVEL     '********'    XXCNAM           30
     C                   END
     C*
     C*
     C*
     C*  END OF SUBROUTINE
     C                   ENDSR
     C*
      *================================================================
      *   SRAGT     -  SUBROUTINE                                     *
      *================================================================
     C* BEGIN SUBROUTINE
     C*
     C     SRAGT         BEGSR
     C*
     C*  END OF SUBROUTINE
     C                   ENDSR
     C*
      *================================================================
      *   SREMP     -  SUBROUTINE                                     *
      *================================================================
     C* BEGIN SUBROUTINE
     C*
     C     SREMP         BEGSR
     C*
     C*  END OF SUBROUTINE
     C                   ENDSR
     C*
      *================================================================
      *   SREMPD    -  SUBROUTINE                                     *
      *================================================================
     C* BEGIN SUBROUTINE
     C*
     C     SREMPD        BEGSR
     C*
     C*  END OF SUBROUTINE
     C                   ENDSR
     C*
      *================================================================
      *   SRLOSS    -  SUBROUTINE                                     *
      *================================================================
     C* BEGIN SUBROUTINE
     C*
     C     SRLOSS        BEGSR
     C*
     C*  END OF SUBROUTINE
     C                   ENDSR
     C*
      *================================================================
      *   SRPROV    -  SUBROUTINE                                     *
      *================================================================
     C* BEGIN SUBROUTINE
     C*
     C     SRPROV        BEGSR
     C*
     C*  END OF SUBROUTINE
     C                   ENDSR
     C*
     C*================================================================
