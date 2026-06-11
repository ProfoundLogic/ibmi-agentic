     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W84X1R - ERM-6 REPORT
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  THIS PROGRAM WILL GENERATE AN EXPERIENCE RATING MODIFIER     *
     H*  REPORT FOR EACH EMPLOYER THAT HAS RECORDS IN THE BSI-5       *
     H*  WORKFILE (WXB5P).  THE BSI-5 WORKFILE IS USED IN CONJUCTION  *
     H*  WITH THE BSI-17 WORKFILE (WXB7P) TO GENERATE A LISTING OF    *
     H*  ALL AUDITED PAYROLLS AND ASSOCIATED CLAIM INFORMATION FOR    *
     H*  EACH EMPLOYER FOR WHATEVER FUND YEARS WERE SPECIFIED ON THE  *
     H*  REPORT PROMPT.  EACH REPORT WILL CONSIST OF AN INSTRUCTION   *
     H*  SHEET (QSYSPRT) WITH THE DATE AND EMPLOYER NAME AT THE       *
     H*  BOTTOM AND ONE OR MORE DATA SHEETS (QSYSPRT1) WITH AUDITED   *
     H*  PAYROLLS AND LOSS INFORMATION PRINTED.                       *
     H*                                                               *
     H*  CALLED BY: W84X1C                                            *
     H*                                                               *
     H*  CALLS TO:  NONE                                              *
     H*                                                               *
     H*  PROGRAMMER: C:09                                             *
     H*  CREATED:    06/03/93          PROJECT: 45AAI                 *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  91   DIFFERENT INFORMATION IS PRINTED ON THE DATA SHEET      *
     H*       DEPENDING ON WHETHER BSI-17 SUMMARY RECORD IS EXCESS    *
     H*       OR NON-EXCESS.  ('0' - NON-EXCESS, '1' - EXCESS).       *
     H*  92   USED TO ENSURE THAT THE PAYROLL AMOUNT IS ONLY PRINTED  *
     H*       ON THE FIRST LINE FOR AN EMPLOYER/CLASS CODE.           *
     H*  99   REPORT IS BEING RUN FOR ONE OR ALL EMPLOYERS            *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*                                                               *
     H*  QSYSPRT - USES THE FRONT OVERLAY - RESOURCE/ERM6_451         *
     H*            OVERFLOW AT 57, 15 CPI, FORMSIZE WIDTH(127)        *
     H*                                                               *
     H*  QSYSPRT - USES THE FRONT OVERLAY - RESOURCE/ERM6_2           *
     H*            OVERFLOW AT 57, 15 CPI, FORMSIZE WIDTH(127)        *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/25/11  C:246 SUPPORT 75005 CLT - 71                        *
     H*                 Move the claim number to file as NCCI as '*'  *
     H*                 is not valid on column 7 on ERM-6 form when   *
     H*                 claim identification number is 1.             *
     H*                                                               *
     H* 12/26/07 C:15  Support: WC071/61618                           *
     H*                Added code to clear B7NCCI when printing just  *
     H*                the BSI-5 data.                                *
     H*                                                               *
     H* 01/23/06 C:15  Support: WC071/50451                           *
     H*                Chgd code to include claims with an acc date   *
     H*                equal to the B5TODT.                           *
     H*                                                               *
     H* 02/24/05 C:15  Support: WC071/45912                           *
     H*                Chgd code added on 2/4/05 to use different     *
     H*                indicator.  *IN99 is used for emp# selection.  *
     H*                                                               *
     H* 02/04/05 C:15  Project: WC1268                                *
     H*               -Reformated output for new version of form.     *
     H*               -Added state of coverage, for new form.         *
     H*                                                               *
     H* 08/10/00 C:61  SUPPORT 23494                                  *
     H*                Don't print address info on last page.         *
     H*                                                               *
     H* 06/02/00 C:61  PROJECT 71WDJ                                  *
     H*                Reformat output to match new overlays.         *
     H*                                                               *
     H* 12/14/99  C:35   PROJECT: SUPPORT WA071 #21004                *
     H*                  CHANGE TO USE V3 FIELD FOR RISK ID.          *
     H*                                                               *
     H* 03/23/99  C:90  PROJECT:SUPPORT WA071/17096                   *
     H*                 LOAD THE CORRECT INJURY TYPE FOR NON-EXCESS   *
     H*                 CLAIMS.                                       *
     H*                                                               *
     H* 02/26/99  C:90  PROJECT:SUPPORT WA071/13794                   *
     H*                 REVISE POSITIONING OF INJURY TYPE AND CLAIM   *
     H*                 VALUES.                                       *
     H*                                                               *
     H* 07/23/98  C:35  PROJECT:SUPPORT                               *
     H*                 CORRECT PRINTING OF YEAR/MONTH.               *
     H*                                                               *
     H*****************************************************************
     FWXB5L1    IF   E           K DISK
     FWXB7L3    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PB701)
     F*** Begin add *** 02/04/05 **************************************
     F                                     IGNORE(PRS01)
     F*** End add ***** 02/04/05 **************************************
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F*** Begin add *** 02/04/05 **************************************
     FWDELL1    IF   E           K DISK
     FWMRSL     IF   E           K DISK
     F*** End add ***** 02/04/05 **************************************
     FQSYSPRT   O    F  127        PRINTER OFLIND(*INOA)
     FQSYSPRT1  O    F  127        PRINTER OFLIND(*INOF)
      ***** BEG ADD ******* 06/02/00 **********************************
      *

     D                 DS
     D  XXZIP                  1      9  0
     D  XXZIP5                 1      5
     D  XXZIP4                 6      9
      ***** END ADD ******* 06/02/00 **********************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  PD                     1     90  2
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
      **** BEGIN ADD **** 07/23/98 *********************************************
     D                 DS
     D  B5EFDT                 1      7  0
     D  XXYRMO                 2      5  0
      **** END   ADD **** 07/23/98 *********************************************

     I*

     C*
     C*  KEY LIST FOR THE BSI-5 WORKFILE
     C*
     C     B5KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C     B5KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*  KEY LIST FOR THE BSI-7 WORKFILE
     C*
     C     B7KEY         KLIST
     C                   KFLD                    B5CO#
     C                   KFLD                    B5FND
     C                   KFLD                    B5EMP#
     C                   KFLD                    B5FYR
     C                   KFLD                    B5CLAS
     C                   KFLD                    B5EFDT
     C*
     C     B7KY1         KLIST
     C                   KFLD                    B5CO#
     C                   KFLD                    B5FND
     C                   KFLD                    B5EMP#
     C                   KFLD                    B5FYR
     C                   KFLD                    B5CLAS
     C*
     C*  KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    B5CO#
     C                   KFLD                    B5FND
     C                   KFLD                    B5EMP#
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    B7CO#
     C                   KFLD                    B7FND
     C                   KFLD                    B7FYR
     C                   KFLD                    B7CASE
     C*
     C*  KEY LIST FOR THE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*** Begin add *** 02/04/05 **************************************
     C*  Key list for the Employer Detail File
     C*
     C     ELKY          KLIST
     C                   KFLD                    B5CO#
     C                   KFLD                    B5FND
     C                   KFLD                    B5FYR
     C                   KFLD                    B5EMP#
      *
      *   Key list for the Rate Set Code File
      *
     C     RSKY          KLIST
     C                   KFLD                    B5CO#
     C                   KFLD                    ELRTCD
     C*** End add ***** 02/04/05 **************************************
     C*
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMEMP#        SVEMP#
      ***** BEG ADD ******* 06/02/00 **********************************
     C     *LIKE         DEFINE    EMPAD1        XXADR
     C     *LIKE         DEFINE    EMPST         XXST
      ***** END ADD ******* 06/02/00 **********************************
     C     *LIKE         DEFINE    RTKEY         XXKEY
     C     *LIKE         DEFINE    CMSTAT        XXSTAT
      ***** BEG ADD ******* 01/25/11 **********************************
     C     *LIKE         DEFINE    CMSTAT        CLSTAT
     C     *LIKE         DEFINE    CMCASE        CLCASE
      ***** END ADD ******* 01/25/11 **********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQEFDT            8
      ***** BEG ADD ******* 06/02/00 **********************************
      *
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      ***** END ADD ******* 06/02/00 **********************************
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*MDY'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
      ***** BEG ADD ******* 01/25/11 **********************************
     C                   MOVE      '1'           *IN81
     C                   MOVEL     '*'           CLSTAT
      ***** END ADD ******* 01/25/11 **********************************
     C*  RUN FOR ONE OR ALL EMPLOYERS
     C*
     C     QQEMP#        IFNE      *ZEROS
     C                   MOVE      '1'           *IN99
     C                   ELSE
     C                   MOVE      '0'           *IN99
     C                   END
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQEFDT        X1EFDT            8 0
     C*
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXEFDT            6 0
     C*
     C                   TIME                    DSDTTM
      **** BEGIN DEL **** 07/23/98 *********************************************
     C******************** Z-ADD*ZEROS    XXYRMO  40
      **** END   DEL **** 07/23/98 *********************************************
     C                   Z-ADD     *ZEROS        XXAGLS           11 0
      ***** BEG DEL ******* 06/02/00 **********************************
     C*************        Z-ADD*ZEROS    XXPDLS 110
     C*************        Z-ADD*ZEROS    XXRSVS 110
      ***** END DEL ******* 06/02/00 **********************************
     C                   Z-ADD     *ZEROS        XXTINC           11 0
     C                   Z-ADD     *ZEROS        XXAAPR           11 0
     C     COKEY         CHAIN     PCO01                              60
     C     FNKEY         CHAIN     PFN01                              60
      ***** BEG DEL ******* 06/02/00 **********************************
     C*********            SELEC
     C*********  FNPST     WHNE *BLANK
     C*********            MOVELFNPST     XXST    2
     C*********  FNMST     WHNE *BLANK
     C*********            MOVELFNMST     XXST    2
     C*********            OTHER
     C*********            MOVELCOST      XXST    2
     C*********            ENDSL
      ***** END DEL ******* 06/02/00 **********************************
     C*
     C*  READ BSI-5 RECORD
     C*
     C                   Z-ADD     *ZEROS        SVEMP#
     C  N99B5KEY         CHAIN     PB501                              40
     C   99B5KY1         CHAIN     PB501                              40
     C     *IN40         DOWEQ     '0'
     C*
     C     B5EMP#        IFNE      SVEMP#
     C*
     C     EMKEY         CHAIN     PEM01                              60
      **** BEGIN CHG **** 12/14/99 *********************************************
     C******************** MOVELEMCOM1    XXRSK#  9
     C                   MOVEL     EMRSK#        XXRSK#            9
      **** END   CHG **** 12/14/99 *********************************************
     C*
     C*** Begin add *** 02/04/05 **************************************
     C*  Get the coverage state.
     C*
     C*** Begin chg *** 02/24/05 **************************************
     C**         ELKY      CHAINPEL01                99
     C**N99      RSKY      CHAINPRS01                99
     C** 99                MOVE *BLANKS   RSJRST
     C     ELKY          CHAIN     PEL01                              60
     C  N60RSKY          CHAIN     PRS01                              60
     C   60              MOVE      *BLANKS       RSJRST
     C*** End chg ***** 02/24/05 **************************************
     C*** End add ***** 02/04/05 **************************************
      ***** BEG ADD ******* 06/02/00 **********************************
     C                   EXSR      SRBHD
      ***** END ADD ******* 06/02/00 **********************************
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
      ***** BEG ADD ******* 06/02/00 **********************************
     C                   EXSR      SRBDET
      ***** END ADD ******* 06/02/00 **********************************
     C                   EXCEPT    DETHD1
     C                   MOVE      '0'           *INOF
     C                   Z-ADD     B5EMP#        SVEMP#
     C                   END
     C*
     C*  READ ALL ASSOCIATED BSI-17 RECORDS
     C*
     C                   MOVE      '0'           *IN92
     C     B7KEY         SETLL     PB701
     C     B7KY1         READE     PB701                                  50
     C     *IN50         IFEQ      '1'
     C*
      **** BEGIN DEL **** 07/23/98 *********************************************
     C******************** MOVELB5EFDT    XXYRMO
      **** END   DEL **** 07/23/98 *********************************************
     C                   Z-ADD     B5AAPR        XXAAPR
     C                   Z-ADD     *ZEROS        B7#CLM
     C                   Z-ADD     *ZEROS        XXAGLS
     C                   MOVE      '0'           *IN91
     C*** Begin add *** 12/26/07 ******************************************
     C                   MOVE      *BLANKS       B7NCCI
     C*** End add ***** 12/26/07 ******************************************
     C*
     C*** Begin dlt *** 12/26/07 ******************************************
     C*  PRINT AN 'F' IF CLAIM IS CLOSED OTHERWISE PRINT AN 'O'
     C*******
     C*******    CMSTAT    IFEQ 'C'
     C*******              MOVE 'F'       XXSTAT
     C*******              ELSE
     C*******              MOVE 'O'       XXSTAT
     C*******              END
     C*** End dlt ***** 12/26/07 ******************************************
     C*
     C     *INOF         IFEQ      '1'
      ***** BEG ADD ******* 06/02/00 **********************************
     C                   EXSR      SRBHD
      ***** END ADD ******* 06/02/00 **********************************
     C                   EXCEPT    HEAD1
     C                   EXCEPT    DETHD1
     C                   MOVE      '0'           *INOF
     C                   END
     C*
      ***** BEG ADD ******* 06/02/00 **********************************
     C                   EXSR      SRBDET
      ***** END ADD ******* 06/02/00 **********************************
     C                   EXCEPT    DETAIL
     C                   MOVE      '1'           *IN92
     C*
     C                   END
     C*
     C*  CONTINUE TO READ ALL ASSOCIATED BSI-17 RECORDS UNTIL THE
     C*  ACCIDENT DATE FALLS OUTSIDE OF THE EFFECTIVE PERIOD FOR
     C*  THE CURRENT BSI-5 CLASS CODE
     C*
     C*** Begin chg *** 01/23/06 **************************************
     C******     B7ACCD    DOWLTB5TODT
     C     B7ACCD        DOWLE     B5TODT
     C*** End chg ***** 01/23/06 **************************************
     C     *IN50         ANDEQ     '0'
      **** BEGIN DEL **** 07/23/98 *********************************************
     C******************** MOVELB5EFDT    XXYRMO
      **** END   DEL **** 07/23/98 *********************************************
     C                   Z-ADD     B5AAPR        XXAAPR
     C*
     C*  CHECK TO SEE IF CLAIM IS EXCESS OR NON-EXCESS
     C*
     C     B7SS#         IFEQ      *ZEROS
     C*
     C*  EXCESS CLAIMS WILL SIMPLY LIST NUMBER OF CLAIMS AND
     C*  AGGREGATE MEDICAL AND INDEMNITY LOSSES
     C*
     C                   MOVE      '0'           *IN91
     C     B7MEDA        ADD(H)    B7INDA        XXAGLS
     C                   ELSE
     C*
     C*  GET CLAIM INFORMATION ASSOCIATED WITH EXCESS CLAIM
     C*
     C                   MOVE      '1'           *IN91
      ***** BEG DEL ******* 06/02/00 **********************************
     C*************        Z-ADD*ZEROS    XXPDLS
     C*************        Z-ADD*ZEROS    XXRSVS
      ***** END DEL ******* 06/02/00 **********************************
     C                   Z-ADD     *ZEROS        XXTINC
     C     CMKEY         CHAIN     PCM01                              60
     C                   DO        10            X                 3 0
     C     RT(X)         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     RT(X)         XXKEY
     C     TBKEY         CHAIN     PRT01                              60
     C     RTTYPE        IFEQ      'C'
     C     RTTYPE        OREQ      'M'
     C*
     C*  ROUND TO NEAREST DOLLAR
     C*
     C                   Z-ADD     PD(X)         XXW112           11 2
     C                   Z-ADD     XXWK11        XXSAVE           11 2
     C     XXW112        IFLT      *ZERO
     C                   MULT      -1            XXW112
     C                   END
     C                   MOVE      XXW112        XX                2 0
     C     XX            IFGE      50
     C                   ADD       1             XXW112
     C                   END
     C                   MOVE      '00'          XXW112
     C     XXSAVE        IFLT      *ZERO
     C     XXW112        MULT      -1            XXW112
     C                   END
      ***** BEG DEL ******* 06/02/00 **********************************
     C*************        ADD  XXW112    XXPDLS
      ***** END DEL ******* 06/02/00 **********************************
     C*
     C*  ROUND TO NEAREST DOLLAR
     C*
     C                   Z-ADD     RR(X)         XXW113           11 2
     C                   Z-ADD     XXW113        XXSAVE
     C     XXW113        IFLT      *ZERO
     C                   MULT      -1            XXW113
     C                   END
     C                   MOVE      XXW113        XX
     C     XX            IFGE      50
     C                   ADD       1             XXW113
     C                   END
     C                   MOVE      '00'          XXW113
     C     XXSAVE        IFLT      *ZERO
     C     XXW113        MULT      -1            XXW113
     C                   END
      ***** BEG DEL ******* 06/02/00 **********************************
     C*************        ADD  XXW113    XXRSVS
      ***** END DEL ******* 06/02/00 **********************************
     C     XXW112        ADD       XXW113        XXWK11           11 0
     C                   ADD       XXWK11        XXTINC
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C*  PRINT AN 'F' IF CLAIM IS CLOSED OTHERWISE PRINT AN 'O'
     C*
     C     CMSTAT        IFEQ      'C'
     C                   MOVE      'F'           XXSTAT
     C                   ELSE
     C                   MOVE      'O'           XXSTAT
     C                   END
     C*
      ***** BEG ADD ******* 01/25/11 **********************************
      * If number claims is 1 and it is non excess then show it is
      * Open or Closed on column 6.
     C                   MOVE      '1'           *IN81
     C     B7#CLM        IFEQ      1
     C     B7SS#         ANDEQ     *ZEROS
     C     CMKEY         CHAIN     PCM01                              60
     C                   Z-ADD     CMCASE        CLCASE
     C                   MOVE      '0'           *IN81
     C     CMSTAT        IFEQ      'C'
     C                   MOVE      'F'           CLSTAT
     C                   ELSE
     C                   MOVE      'O'           CLSTAT
     C                   END
     C                   ELSE
     C                   MOVEL     '*'           CLSTAT
     C                   END
      ***** END ADD ******* 01/25/11 **********************************
     C*
     C     *INOF         IFEQ      '1'
      ***** BEG ADD ******* 06/02/00 **********************************
     C                   EXSR      SRBHD
      ***** END ADD ******* 06/02/00 **********************************
     C                   EXCEPT    HEAD1
     C                   EXCEPT    DETHD1
     C                   MOVE      '0'           *INOF
     C                   END
     C*
      ***** BEG ADD ******* 06/02/00 **********************************
     C                   EXSR      SRBDET
      ***** END ADD ******* 06/02/00 **********************************
     C                   EXCEPT    DETAIL
     C                   MOVE      '1'           *IN92
      ***** BEG ADD ******* 01/25/11 **********************************
     C                   MOVEL     '*'           CLSTAT
     C                   Z-ADD     *ZEROS        CLCASE
     C                   MOVE      '1'           *IN81
      ***** END ADD ******* 01/25/11 **********************************
     C*
     C     B7KY1         READE     PB701                                  50
     C                   END
     C*
     C  N99B5KEY         READE     PB501                                  40
     C   99B5KY1         READE     PB501                                  40
     C                   END
     C*
     C                   MOVE      '1'           *INLR
      ***** BEG ADD ******* 06/02/00 **********************************
      *
      *
      *
      *==============================================================
      * SRBDET - PREPARATION BEFORE PRINT OF DETAIL LINE
      *==============================================================
     C     SRBDET        BEGSR
      *
     C                   Z-ADD     B5EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXB5DT            6 0
      *
     C                   Z-ADD     B5TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTODT            6 0
      *
     C     XXTODT        COMP      *ZERO                                  72
      *
     C                   ENDSR                                                  SRBDET
      *
      *
      *
      *==============================================================
      * SRBHD  - PREPARATION BEFORE PRINT OF HEADER
      *==============================================================
     C     SRBHD         BEGSR
      *
      *  INSTRUCTION SHEET
      *
      *    Zip code
      *
     C                   MOVEL(P)  *BLANKS       X2ZIP            10
      *
     C     FNMZIP        IFNE      *ZERO
     C                   Z-ADD     FNMZIP        XXZIP
      *
     C     XXZIP5        IFNE      '00000'
     C                   MOVEL(P)  XXZIP5        X2ZIP            10
      *
     C     XXZIP4        IFNE      '0000'
     C                   CAT(P)    '-':0         X2ZIP
     C                   CAT(P)    XXZIP4:0      X2ZIP
     C                   ENDIF                                                  XXZIP4
      *
     C                   ENDIF                                                  XXZIP5
      *
     C                   ENDIF                                                  FNMZIP
      *
      *    Phone
      *
     C                   MOVE      FNPH1A        XXAREA            3
     C                   MOVEL     FNPH1         XXPH1             3
     C                   MOVE      FNPH1         XXPH2             4
     C                   EXSR      SRPHN
     C                   MOVEL(P)  XXPHON        XXPH             15
      *
      *    Fax
      *
     C                   MOVE      FNFAXA        XXAREA            3
     C                   MOVEL     FNFAX         XXPH1             3
     C                   MOVE      FNFAX         XXPH2             4
     C                   EXSR      SRPHN
     C                   MOVEL(P)  XXPHON        XXFAX            15
      *
      *  DATA SHEET
      *
      *  Employer address info
      *
      *    Physical address
      *
     C     EMPAD1        IFNE      *BLANKS
     C                   MOVEL(P)  EMPAD1        XXADR
     C                   MOVEL(P)  EMPCTY        XXCTY            20
     C                   MOVEL(P)  EMPST         XXST
     C                   MOVEL(P)  *BLANKS       X1ZIP            10
      *
     C     EMPZIP        IFNE      *ZERO
     C                   Z-ADD     EMPZIP        XXZIP
      *
     C     XXZIP5        IFNE      '00000'
     C                   MOVEL(P)  XXZIP5        X1ZIP            10
      *
     C     XXZIP4        IFNE      '0000'
     C                   CAT(P)    '-':0         X1ZIP
     C                   CAT(P)    XXZIP4:0      X1ZIP
     C                   ENDIF                                                  XXZIP4
      *
     C                   ENDIF                                                  XXZIP5
      *
     C                   ENDIF                                                  EMPZIP
      *
     C                   ELSE
      *
      *    Mailing address
      *
     C                   MOVEL(P)  EMMAD1        XXADR
     C                   MOVEL(P)  EMMCTY        XXCTY
     C                   MOVEL(P)  EMMST         XXST
     C                   MOVEL(P)  *BLANKS       X1ZIP            10
      *
     C     EMMZIP        IFNE      *ZERO
     C                   Z-ADD     EMMZIP        XXZIP
      *
     C     XXZIP5        IFNE      '00000'
     C                   MOVEL(P)  XXZIP5        X1ZIP            10
      *
     C     XXZIP4        IFNE      '0000'
     C                   CAT(P)    '-':0         X1ZIP
     C                   CAT(P)    XXZIP4:0      X1ZIP
     C                   ENDIF                                                  XXZIP4
      *
     C                   ENDIF                                                  XXZIP5
      *
     C                   ENDIF                                                  EMMZIP
      *
     C                   ENDIF                                                  EMPAD1
      *
      *
     C     X1ZIP         COMP      *BLANKS                                71
      *
     C                   ENDSR                                                  SRBHD
      *
      *
      *
      *================================================================
      * SRPHN - Format phone number for output
      *================================================================
     C     SRPHN         BEGSR
      *
     C     XXAREA        IFNE      '000'
     C     XXPH1         ANDNE     '000'
     C                   MOVEL(P)  '('           XXPHON           20
     C                   CAT(P)    XXAREA:0      XXPHON
     C                   CAT(P)    ')':0         XXPHON
     C                   CAT(P)    XXPH1:1       XXPHON
     C                   CAT(P)    '-':0         XXPHON
     C                   CAT(P)    XXPH2:0       XXPHON
      *
     C                   ELSE
      *
     C                   MOVEL(P)  *BLANKS       XXPHON
     C                   ENDIF
      *
     C                   ENDSR                                                  SRPHN
      *
      *
      *
      ***** END ADD ******* 06/02/00 **********************************
     C*
      ***** BEG DEL ******* 06/02/00 **********************************
      ***********
     O***********
     O****INSTRUCTION SHEET
     O***********
     O*QSYSPRT*E** 58           HEAD
     O***********              EMNAME   124
     O********E** 61           HEAD
     O***********              DSDATE    20 '  /  /  '
     O***********              FNNAME   102
     O***********
     O****DATA*SHEET
     O***********
     O*QSYSPRT1E**  9           HEAD1
     O***********                        16 'POLICY #:'
     O***********              EMFND     21
     O***********                        22 '-'
     O***********              EMEMP#    31
     O*QSYSPRT1E** 10           HEAD1
     O***********              EMNAME    64
     O***********              XXST     102
     O********E** 12           HEAD1
     O***********              XXRSK#    44
     O***********              XXEFDT   118 '  /  /  '
     O********E** 19           DETHD1
     O********E**1             DETAIL
     O***********              XXYRMO    15 '  /  '
     O***********              B5CLAS4   25
     O***********              B5SPEC    27
     O***********     N92      XXAAPRJ   41
     O***********      91      CMCASE4   51
     O***********      91      CMNCCI    66
     O***********      91                67 '-'
     O***********      91      XXSTAT    68
     O***********     N91      B7#CLM3   51
      *****BEGIN*CHG **** 02/26/99 ************************************
      *****BEGIN*CHG **** 03/23/99 ************************************
     O***********     N91      B7NCCI    61
     O*************** N91      CMNCCI    61
      *****END***CHG **** 03/23/99 ************************************
     O*************** N91      XXAGLSJ   69
      *****END***CHG **** 02/26/99 ************************************
     O***********      91      XXPDLSJ   85
     O***********      91      XXRSVSJ  100
     O***********      91      XXTINCJ  119
      *****BEGIN*ADD **** 02/26/99 ************************************
     O***********     N91      XXAGLSJ  119
      *****END***ADD **** 02/26/99 ************************************
      ***** END DEL ******* 06/02/00 **********************************
      ***** BEG ADD ******* 06/02/00 **********************************
      *
      *   INSTRUCTION SHEET
      *
     OQSYSPRT   E            HEAD           2 33
      *** Begin add *** 02/04/05 **************************************
     O          E            HEAD             36
      *** End add ***** 02/04/05 **************************************
     O                       EMNAME              88
      *
      ***** BEG DEL ******* 08/10/00 **********************************
     O******* E  1             HEAD
     O*******                  FNMAD1    55
     O*******                  FNMCTY   118
      *******
     O******* E  1             HEAD
     O*******                  FNMST     15
     O*******                  X2ZIP     38
     O*******                  XXPH      68
     O*******                  XXFAX     91
      ***** END DEL ******* 08/10/00 **********************************
      *
      *   DATA SHEET
      *
      *** Begin dlt *** 02/04/05 **************************************
     O*****QSYSPRT1E  3 2           HEAD1
     O*****                            +008 'POLICY #:'
     O*****                    EMFND   +001
     O*****                            +000 '-'
     O*****                    EMEMP#  +000
      *
     O*****   E  2             HEAD1
     O*****                    EMNAME    64
      *
     O*****   E  3             HEAD1
     O*****                    XXADR     66
     O*****                    XXCTY     99
     O*****                    XXST     112
      *
     O*****   E  2             HEAD1
     O*****           N71      X1ZIP     21
     O*****                    XXRSK#    58
     O*****                    XXEFDT   103 '  /  /  '
      *
     O*****   E  2             HEAD1
     O*****                    EMFEIN    52
      *
      *
     O*****   E   21           DETHD1
      *
      *
     O*****   E  1             DETAIL
     O*****                    XXB5DT    17 '  /  /  '
     O*****           N72      XXTODT    28 '  /  /  '
     O*****                    B5CLAS4   35
     O*****                    B5SPEC    37
     O*****           N92      XXAAPRJ   60
      *
     O*****            91      CMCASE4   72
     O*****            91      CMNCCI    84
     O*****            91      XXSTAT    92
     O*****            91      XXTINCJ  115
      *
     O*****           N91      B7#CLM3   72
     O*****           N91      B7NCCI    84
     O*****           N91                92 '*'
     O*****           N91      XXAGLSJ  115
      *
      ***** END ADD ******* 06/02/00 **********************************
      *** End dlt ***** 02/04/05 **************************************
      *** Begin add *** 02/04/05 **************************************
     OQSYSPRT1  E            HEAD1          3  2
     O                                         +008 'POLICY #:'
     O                       EMFND             +001
     O                                         +000 '-'
     O                       EMEMP#            +000
      *
     O          E            HEAD1          3 13
     O                       EMNAME              67
      *
     O          E            HEAD1          2
     O                       XXADR               70
     O                       XXCTY              103
     O                       XXST               116
      *
     O          E            HEAD1          3
     O              N71      X1ZIP               26
     O                       XXRSK#              61
     O                       XXEFDT             110 '  /  /  '
      *
     O          E            HEAD1          2
     O                       EMFEIN              55
     O                       RSJRST              99
      *
      *
     O          E            DETHD1           30
      *
      *
     O          E            DETAIL         1
     O                       XXB5DT              21 '  /  /  '
     O              N72      XXTODT              32 '  /  /  '
     O                       B5CLAS        4     39
     O                       B5SPEC              41
     O              N92      XXAAPR        J     63
      *
     O               91      CMCASE        4     75
     O               91      CMNCCI              87
     O               91      XXSTAT              95
     O               91      XXTINC        J    118
      *
     O              N91      B7#CLM        3     75
      *** BEG ADD ***** 01/25/11 **************************************
     O              N91N81   CLCASE        4     75
      *** END ADD ***** 01/25/11 **************************************
     O              N91      B7NCCI              87
      *** BEG CHG ***** 01/25/11 **************************************
     O***             N91                95 '*'
     O              N91      CLSTAT              95
      *** END CNG ***** 01/25/11 **************************************
     O              N91      XXAGLS        J    118
      *** End add ***** 02/04/05 **************************************
