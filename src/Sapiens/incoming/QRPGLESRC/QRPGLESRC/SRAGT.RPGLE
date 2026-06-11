     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SRAGT - RETRIEVE AGENT INFORMATION.
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                       *
     H**                                                              *
     H**   THIS PROGRAM WILL RETURN AGENT INFORMATION FIELDS.         *
     H**                                                              *
     H* PROGRAMMER: C:65                                              *
     H* CREATED:    06/07/04            PROJECT: 132WAC               *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H* 61  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/22/21 C: 079 Project: WC2840                               *
     H*                 Add email address logic                       *
     H*                                                               *
     H*****************************************************************
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWMAPL     IF   E           K DISK

     D                 DS
     D  XXZIP                  1      9  0
     D  XXZIP5                 1      5
     D  XXZIP4                 6      9
     d*** Beg add *** 12/22/21 *************************************************
     d psds          esds                  extname(szq1p)
     d*** End add *** 12/22/21 *************************************************

     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEI#
     C                   KFLD                    XXMOD
     C*
     C* Key List for the Agent Producer Master File
     C*
     C     APKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEI#
     C                   KFLD                    XXMOD
     C                   KFLD                    XXPROD
     C*
     C     *LIKE         DEFINE    AACO#         XXCO#
     C     *LIKE         DEFINE    AAFND         XXFND
     C     *LIKE         DEFINE    AAAGT#        XXAGT#
     C     *LIKE         DEFINE    APPROD        XXPROD
     C     *LIKE         DEFINE    AAFEIN        XXFEI#
     C     *LIKE         DEFINE    AAMOD         XXMOD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQAGT#            5
     C                   PARM                    QQPROD            5
     C                   PARM                    QQAGNM           50
     C                   PARM                    QQAMAD           50
     C                   PARM                    QQAMCZ           50
     C                   PARM                    QQAPAD           50
     C                   PARM                    QQAPCZ           50
     C                   PARM                    QQPRNM           50
     C                   PARM                    QQMCTY           30
     C                   PARM                    QQMST             2
     C                   PARM                    QQMZIP            9
     C                   PARM                    QQPCTY           30
     C                   PARM                    QQPST             2
     C                   PARM                    QQPZIP            9
     C                   PARM                    QQARE1            3
     C                   PARM                    QQPHN1            7
     C*** Beg Add *** 12/22/21 *************************************************
     C                   PARM                    QQMAIL           60
     C*** End Add *** 12/22/21 *************************************************
     C*
     C*    SET UP AND INITALIZE
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQAGT#        XXAGT#
     C                   MOVE      QQPROD        XXPROD
     C                   MOVE      *BLANKS       QQAGNM
     C                   MOVE      *BLANKS       QQAMAD
     C                   MOVE      *BLANKS       QQAMCZ
     C                   MOVE      *BLANKS       QQAPAD
     C                   MOVE      *BLANKS       QQAPCZ
     C                   MOVE      *BLANKS       QQPRNM
     C                   MOVE      *BLANKS       QQMCTY
     C                   MOVE      *BLANKS       QQMST
     C                   MOVE      *ZEROS        QQMZIP
     C                   MOVE      *BLANKS       QQPCTY
     C                   MOVE      *BLANKS       QQPST
     C                   MOVE      *ZEROS        QQPZIP
     C                   MOVE      *ZEROS        QQARE1
     C                   MOVE      *ZEROS        QQPHN1
     C*
     C                   MOVE      *BLANKS       XXAGNM           50
     C                   MOVE      *BLANKS       XXAMAD           50
     C                   MOVE      *BLANKS       XXAMCZ           50
     C                   MOVE      *BLANKS       XXAPAD           50
     C                   MOVE      *BLANKS       XXAPCZ           50
     C                   MOVE      *BLANKS       XXPRNM           50
     C                   MOVE      *BLANKS       XXMCTY           30
     C                   MOVE      *BLANKS       XXMST             2
     C                   MOVE      *ZEROS        XXMZIP            9
     C                   MOVE      *BLANKS       XXPCTY           30
     C                   MOVE      *BLANKS       XXPST             2
     C                   MOVE      *ZEROS        XXPZIP            9
     C                   MOVE      *ZEROS        XXARE1            3
     C                   MOVE      *ZEROS        XXPHN1            7
     C*
     C     AAKY          CHAIN     PAA01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     AAFEIN        XXFEI#
     C                   MOVEL     AAMOD         XXMOD
     C                   ELSE
     C                   MOVE      *ZEROS        XXFEI#
     C                   MOVE      *ZEROS        XXMOD
     C                   END
     C*
     C     AGKY          CHAIN     PAG01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     AGNAME        XXAGNM
     C                   MOVEL     AGMAD1        XXAMAD
     C                   MOVE      AGMZIP        XXZIP
     C                   MOVEL     AGMCTY        XXAMCZ
     C                   CAT       ',':0         XXAMCZ
     C                   CAT       AGMST:1       XXAMCZ
     C                   CAT       XXZIP5:1      XXAMCZ
     C                   MOVEL     AGMCTY        XXMCTY
     C                   MOVEL     AGMST         XXMST
     C                   MOVE      AGMZIP        XXMZIP
     C                   MOVEL     AGPAD1        XXAPAD
     C                   MOVE      AGPZIP        XXZIP
     C                   MOVEL     AGPCTY        XXAPCZ
     C                   CAT       ',':0         XXAPCZ
     C                   CAT       AGPST:1       XXAPCZ
     C                   CAT       XXZIP5:1      XXAPCZ
     C                   MOVEL     AGPCTY        XXPCTY
     C                   MOVEL     AGPST         XXPST
     C                   MOVE      AGPZIP        XXPZIP
     C                   MOVE      AGARE1        XXARE1
     C                   MOVE      AGPHN1        XXPHN1
     C*** Beg Add *** 12/22/21 *************************************************
       If Q1prms > 18;
          QQmail = AGeMal;
       Endif;
     C*** End Add *** 12/22/21 *************************************************
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     APKY          CHAIN     PAP01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     APFSN         IFNE      *BLANKS
     C                   MOVEL     APFSN         XXPRNM
     C                   CAT       APLSN:1       XXPRNM
     C                   ELSE
     C                   MOVEL     APLSN         XXPRNM
     C                   ENDIF                                                  APFSN IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   MOVE      XXAGNM        QQAGNM
     C                   MOVE      XXAMAD        QQAMAD
     C                   MOVE      XXAMCZ        QQAMCZ
     C                   MOVE      XXAPAD        QQAPAD
     C                   MOVE      XXAPCZ        QQAPCZ
     C                   MOVE      XXPRNM        QQPRNM
     C                   MOVE      XXMCTY        QQMCTY
     C                   MOVE      XXMST         QQMST
     C                   MOVE      XXMZIP        QQMZIP
     C                   MOVE      XXPCTY        QQPCTY
     C                   MOVE      XXPST         QQPST
     C                   MOVE      XXPZIP        QQPZIP
     C                   MOVE      XXARE1        QQARE1
     C                   MOVE      XXPHN1        QQPHN1
     C*
     C                   MOVE      '1'           *INLR
     C*
