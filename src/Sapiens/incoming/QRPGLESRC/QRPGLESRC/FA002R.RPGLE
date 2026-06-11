     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - FA002R - WRITE/UPDATE WCMC CLAIM DEMO INFO BASED ON WCMS DATA
     H*                FEE SHEDULE VERSION
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA002R                                            *
     H*    CREATED: 11/15/96  C:45                                    *
     H*    DESCRIPTION: WRITE/UPDATE WCMC CLAIM DEMO INFO BASED ON    *
     H*                 WCMS DATA                                     *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     h*                                                               *
     h*  05/09/16 C:300 Project: 114WXV                               *
     h*                 Add Cell Number to Claimant Demographics.     *
     H*                                                               *
     H* 01/07/16 C:062 Support: 39071(133/099)                        *
     H*                Claim demo error due to first FEOD             *
     H*                                                               *
     H* 12/16/15 C:180 Support: 32115                                 *
     H*          C:306 (Retrofit) - Prevent dup records in file FMCDP.*
     H*                                                               *
     H* 10/20/03 C:53  Project: WC909                                 *
     H*                Added country info.                            *
     H*                                                               *
     H* 01/31/00 C:62 Req/Proj: MCMNT                                 *
     H*               CALL FA704R TO UPDATE THE DEMOGRAPHICS INFO OF  *
     H*               OTHER FILES LIKE FAEOP AND FALTP.               *
     H*                                                               *
     H* 10/02/98 C:62 Req/Proj: 87MFV                                 *
     H*               DEFAULTING COUNTRY TO 'USA'.                    *
     H*                                                               *
     H* 02/17/97 C:53 Req/Proj: 87MAZ                                 *
     H*               ACTIVATED V3.5 CODE, CORRECTED CODE             *
     H*
     H* 12/26/97 C:53 Req/Proj: 87MAZ  SUPPORT#: 9550
     H*               ADD CALL TO FA704R TO UPDATE CLAIM RECORDS
     H*               WITH NAME CHANGE IF NEEDED
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       20      PARMS PASED WERE ALL FILLED                     *
     H*       60      GENERAL USAGE (CHAIN)                           *
     H*                                                               *
     H*****************************************************************
     F*
     FFMCDL     UF A E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  CLAIMANT DEMOGRAPHICS FILE KEY LIST
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQRTCD            1             1
     C                   PARM                    QQSS#             9             2
     C                   PARM                    QQACDT            7             3
     C                   PARM                    QQCO#             3             4
     C                   PARM                    QQSUB#            5             5
     C                   PARM                    QQFND             3             6
     C                   PARM                    QQFYR             3             7
     C                   PARM                    QQCASE            7             8
     C*
     C                   PARM                    QQLSN            30             9
     C                   PARM                    QQFSN            15            10
     C                   PARM                    QQMID             1            11
     C                   PARM                    QQADR1           40            12
     C                   PARM                    QQADR2           40            13
     C                   PARM                    QQCTY            25            14
     C                   PARM                    QQST              2            15
     C                   PARM                    QQZIP             9            16
     C                   PARM                    QQDTOB            7            17
     C                   PARM                    QQSEX             1            18
     C                   PARM                    QQAREA            3            19
     C                   PARM                    QQPHN#            7            20
     C*** BEGIN ADD V4
     C                   PARM                    QQMSTS            1
     C                   PARM                    QQWARA            3
     C                   PARM                    QQWPH#            7
     C*** END   ADD V4
     C*** BEG ADD *** 10/20/03 ****************************
24   C                   PARM                    QQCTRY            3
25   C                   PARM                    QQADR3           40
     C*** END ADD *** 10/20/03 ****************************
     C*** BEG ADD *** 05/09/16 ****************************
26   C                   PARM                    QQCARA            3
27   C                   PARM                    QQCPHN            7
     C*** END ADD *** 05/09/16 ****************************
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
     C     *LIKE         DEFINE    CDSS#         XXSS#
     C                   MOVE      QQSS#         XXSS#
     C*
     C*** ATTEMPT TO GET CLAIM DEMOGRAPHICS RECORD
     C*
     C     CDKEY         CHAIN     PCD01                              60        ADD 02/17/97
     C*
     C*** BEG ADD *** 12/26/97 *****************************
     C*** IF NAME HAS CHANGED CALL API TO CYCLE THRU CLAIM RCDS
     C*
     C     *IN60         IFEQ      '0'
     C     QQLSN         IFNE      CDLSN
     C     QQFSN         ORNE      CDFSN
     C     QQMID         ORNE      CDMID
     C                   CALL      'FA704R'
     C                   PARM                    QQSS#
     C                   PARM                    QQLSN
     C                   PARM                    QQFSN
     C                   PARM                    QQMID
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 12/26/97 *****************************
     C*
     C*
     C*** LOAD THE FIELDS FOR WRITE / UPDATE
     C*
     C                   MOVE      QQSS#         CDSS#
     C                   MOVE      QQLSN         CDLSN
     C                   MOVE      QQFSN         CDFSN
     C                   MOVE      QQMID         CDMID
     C                   MOVE      QQADR1        CDADR1
     C                   MOVE      QQADR2        CDADR2
     C                   MOVE      QQCTY         CDCTY
     C                   MOVE      QQST          CDST
     C                   MOVE      QQZIP         CDZIP
     C*
     C***                  MOVE QQDTOB    XXDTOB  80
     C*
     C***                  Z-ADDXXDTOB    SRDTI
     C***                  CALL 'SRCVTDAT'DTCYMD
     C***                  MOVELSRDTO     CDDTOB
     C*
     C                   MOVE      QQDTOB        CDDTOB
     C*
     C     CDDTOB        IFEQ      *ZEROS
     C                   Z-ADD     0500101       CDDTOB
     C                   ENDIF
     C*
     C                   MOVE      QQSEX         CDSEX
     C*
     C*** THE FOLLOWING 2 FIELDS ARE FOR V3.5 ONLY
     C*
     C                   MOVE      QQAREA        CDAREA
     C                   MOVE      QQPHN#        CDPHN#
     C*
     C*** BEGIN ADD *** 10/14/97 **************************************
     C     Q1PRMS        IFGT      20
     C                   MOVE      QQMSTS        CDMSTS
     C                   MOVE      QQWARA        CDWARA
     C                   MOVE      QQWPH#        CDWPH#
     C                   ENDIF
     C*** END   ADD *** 10/14/97 **************************************
     C*** BEG DEL *** 10/20/03 ****************************
     C****BEGIN*ADD *** 10/02/98 **************************************
     C**********           MOVEL'USA'     CDCTRY
     C****END***ADD *** 10/02/98 **************************************
     C*** END DEL *** 10/20/03 ****************************
     C*** BEG ADD *** 10/20/03 ****************************
     C     Q1PRMS        IFGT      23
     C*
     C     QQCTRY        IFEQ      *BLANKS
     C                   MOVE      'USA'         CDCTRY
     C*
     C                   ELSE
     C                   MOVE      QQCTRY        CDCTRY
     C                   ENDIF
     C*
     C                   MOVE      QQADR3        CDADR3
     C                   ENDIF
     C*** END ADD *** 10/20/03 ****************************
     C*** BEG ADD *** 05/09/16 ****************************
     C     Q1PRMS        IFGT      25
     C                   MOVE      QQCARA        CDCARA
     C                   MOVE      QQCPHN        CDCPHN
     C                   ENDIF
     C*** END ADD *** 05/09/16 ****************************
     C*
     C*** CREATE THE CONSTANT FIELDS
     C*
     C                   MOVE      'A'           CDSTAT
     C                   MOVE      Q1USER        CDUSER
     C                   MOVE      Q1PGM         CDPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CDDT
     C*
     C*** UPDATE/WRITE CLAIM DEMOGRAPHICS INFORMATION
     C*
      /Free
       //**** Begin Del **** 01/07/16 ************************
       //**** Begin add **** 12/16/15 ************************
       //*FEOD FMCDL ;
       //**** End   add **** 12/16/15 ************************
       //**** End   Del **** 01/07/16 ************************
      /End-Free
     C     *IN60         IFEQ      '1'
     C                   WRITE     PCD01
     C                   ELSE
     C                   UPDATE    PCD01
     C                   ENDIF
      /Free
       //**** Begin add **** 12/16/15 ************************
         FEOD FMCDL ;
       //**** End   add **** 12/16/15 ************************
      /End-Free
     C*** BEGIN ADD *** 01/31/00 **************************************
     C                   CALL      'FA704R'
     C                   PARM      CDSS#         ##SS#             9
     C                   PARM      CDLSN         ##LSN            30
     C                   PARM      CDFSN         ##FSN            15
     C                   PARM      CDMID         ##MID             1
     C*** END   ADD *** 01/31/00 **************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   RETURN
     C*
