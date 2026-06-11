     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE F6251R - EDI 837 TRIGGER API
     H*
     H*****************************************************************
     H* Program description:  This program is called from both the    *
     H*         WCMC and WCMS batch processes.  It determines whether *
     H*         837 log records are created after the EOMB has been   *
     H*         processed or after the EOMB has been paid, based on   *
     H*         the 837 Control file.  To create the log records, the *
     H*         log API (F6250R) is called.                           *
     H*                                                               *
     H* Called from:  F6000C                                          *
     H*               W6000C                                          *
     H*                                                               *
     H* Created: 08/10/99    C:15     Project: KY010                  *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  07/20/07 C:188 SUPPORT: SEQ58659                             *
     H*            OMIT PRECERT RECORDS WHERE (EASYS NE H OR P)       *
     H*                                                               *
     H*  04/23/07 C:188 PROJECT:FL061                                 *
     H*            USE POLICY STATE FOR COMPARE WHEN NOT BLANK        *
     H*                                                               *
     H* 03/26/07 C: 182  Project: MC281                              *
     H*                  Added check for 837 Medical EDI for Denied  *
     H*                  Bills                                       *
     H*                                                               *
     H* 07/17/06  C: 079  PROJ: 135MHP                                *
     H*                   1. USE THE ADJ STATE NOT CLAIM JUR STATE    *
     H*                   2. ONLY SELECT BILLS WHERE EOTAPE = Y       *
     H*                   3. PASS AND ENDING DATE TO W6251R           *
     H*                   4. RESET FAEAP                              *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*
     FFEEXL1    IF   E           K DISK
     FFYEAP     IF   E             DISK
    F*** BEG CHG *** 07/17/06 ****************************************
     F***** FMCML1  IF  E           K        DISK
     FFAEOLG    IF   E           K DISK
    F*** END CHG *** 07/17/06 ****************************************
     F*

     D*================================================================
     D*================================================================
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*================================================================
     C*  FEEXL - EDI Control File
     C*
     C     EXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTSET
     C*** BEG ADD *** 04/23/07 ****************************************
     C     *LIKE         DEFINE    EOAST         EDITST
     C*** END ADD *** 04/23/07 ****************************************
     C*
    C*** BEG CHG *** 07/17/06 ****************************************
     C********** CMKEY     KLIST
     C     EOKEY         KLIST
    C*** END CHG *** 07/17/06 ****************************************
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
    C*** BEG ADD *** 07/17/06 ****************************************
     C                   KFLD                    EAEOB#
    C*** END ADD *** 07/17/06 ****************************************
     C*
     C*  Entry parm list
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3            company
     C                   PARM                    QQPRDT            8            process date
     C                   PARM                    QQCPGM            1            CALL PGM FLG
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
     C*
     C     *LIKE         DEFINE    EXCO#         XXCO#
     C     *LIKE         DEFINE    EXTSET        XXTSET
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      '837'         XXTSET
     C*
     C                   CLEAR                   PEX01
     C                   CLEAR                   PEA01
    C*** BEG CHG *** 07/17/06 ****************************************
     C*******************  CLEARPCM01
     C                   CLEAR                   PEO01
    C*** END CHG *** 07/17/06 ****************************************
     C*
     C*****************************************************************
     C*  Read all 837 control records to process all states.
     C*
     C     EXKY          CHAIN     PEX01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C*  If the log record is created after the completion of the bill,
     C*  call the subroutine which reads all EOMBs for claims with an
     C*  adjudication state equal to the control file state (EXJRST)
     C*  and the completion date equals the batch date (QQPRDT).
     C*
     C     EXLOG         IFEQ      '1'
     C     QQCPGM        ANDEQ     'F'
     C                   EXSR      MCSR
     C                   ENDIF
     C*
     C*  If the log record is created after the completion of the WCMS
     C*  check, call the WCMS 837 trigger pgm which will read all pmts
     C*  on EOMBs which were processed on the batch date.
     C*
     C     EXLOG         IFEQ      '2'
     C     QQCPGM        ANDEQ     'W'
     C                   EXSR      WCSR
     C                   ENDIF
     C*
     C*** BEG ADD ***** 03/26/07 **************************************
     C*
     C* For denied Bills. If the log record is created after the completionf the
     C* of the bill call the subroutine which reads all EOMBs for claims
     C* with an adjudication state equal to the control file state (EXJRST)
     C* and the completion date euals the batch date (QQPRDT).
     C*
     C     EXPDNY        IFEQ      'Y'
     C     QQCPGM        ANDEQ     'F'
     C                   EXSR      MCSR
     C                   ENDIF
    C*
    C*** END ADD ***** 03/26/07 **************************************
    C*
     C     EXKY          READE     PEX01                                  99
     C                   ENDDO
     C*
     C*  End the program.
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C*  MCSR - This subroutine calls the 837 Log API to create 837
     C*  log records for EOMBs which have been completed.
     C*****************************************************************
     C     MCSR          BEGSR
     C*
    C*** BEG ADD *** 07/17/06 ****************************************
     C     1             SETLL     PEA01
    C*** END ADD *** 07/17/06 ****************************************
     C                   READ      PEA01                                  98
     C     *IN98         DOWEQ     *OFF
     C*
    C*** BEG ADD *** 07/20/07 ****************************************
     C     EASYS         IFEQ      'H'
     C     EASYS         OREQ      'P'
    C*** END ADD *** 07/20/07 ****************************************
     C*
    C*** BEG CHG *** 07/17/06 ****************************************
     C********** CMKEY     CHAINPCM01                97
     C     EOKEY         CHAIN     PEO01                              97
    C*** END CHG *** 07/17/06 ****************************************
     C*
     C*** BEG ADD *** 04/23/07 ****************************************
     C     EOPOST        IFNE      *BLANKS
     C                   MOVEL     EOPOST        EDITST
     C                   ELSE
     C                   MOVEL     EOAST         EDITST
     C                   ENDIF
     C*** END ADD *** 04/23/07 ****************************************
     C*
     C     *IN97         IFEQ      *OFF
     C*
    C*** BEG CHG *** 07/17/06 ****************************************
     C********** CMAST     ANDEQEXJRST
    C*** BEG CHG *** 04/23/07 ****************************************
     C*****      EOAST     ANDEQEXJRST
     C     EDITST        ANDEQ     EXJRST
    C*** END CHG *** 04/23/07 ****************************************
     C     EOTAPE        ANDEQ     'Y'
    C*** END CHG *** 07/17/06 ****************************************
     C*
     C                   MOVE      EACO#         QQCO#
     C                   MOVE      EASUB#        QQSUB#
     C                   MOVE      EAFND         QQFND
     C                   MOVE      EAFYR         QQFYR
     C                   MOVE      EACASE        QQCASE
     C                   MOVE      EAEOB#        QQEOB#
     C                   MOVE      EARCN         QQRCN
     C*
    C*** BEG ADD ***** 03/26/07 ******************************************
     C*
     C     EXPDNY        IFEQ      'Y'
     C     EAPAID        ANDGT     0
     C*
     C                   ELSE
     C*
    C*** END ADD ***** 03/26/07 ******************************************
     C*
     C                   CALL      'F6250R'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQEOB#            9
     C                   PARM                    QQRCN             3
     C                   PARM      *BLANK        QQACTF            1
     C                   PARM      *BLANK        QQRTCD            1
     C*
    C*** BEG ADD ***** 03/26/07 ******************************************
     C*
     C                   ENDIF
     C*
    C*** END ADD ***** 03/26/07 ******************************************
     C*
     C                   ENDIF
     C*
    C*** BEG ADD ***** 07/20/07 ******************************************
     C                   ENDIF                                                  EASYS <> H OR P
    C*** END ADD ***** 07/20/07 ******************************************
     C*
     C                   READ      PEA01                                  98
     C                   ENDDO
     C*
     C                   ENDSR
     C*****************************************************************
     C*  WCSR - This subroutine calls the WCMS 837 trigger pgm to
     C*  create 837 log records for EOMBs which have been paid.
     C*****************************************************************
     C     WCSR          BEGSR
     C*
     C                   CALL      'W6251R'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQPRDT            8
     C                   PARM      EXJRST        QQJRST            2
     C*
     C                   ENDSR
     C*****************************************************************
