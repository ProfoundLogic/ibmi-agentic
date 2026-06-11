     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     H/define CONVERT
     H*
     H/TITLE W7413R5 - PROCESS ADDITIONAL DOCUMENTS FOR NY/MA POLICIES
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM WILL PRINT ADDITIONAL DOCUMENTS FOR NY/MA  **
     H**     POLICIES INCLUDING:                                     **
     H**     COVER LETTER                                            **
     H**     NY POSTING NOTICE  (REMOVED 10/20/06)                   **
     H**     MA POSTING NOTICE                                       **
     H**     NY CERTIFICATE (THROUGH W7511R CALL)                    **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     W7413C5 - NY/MA DOCUMENT OVERRIDE                       **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     W7511R - CERTIFICATE HOLDERS PROGRAM                    **
     H**                                                             **
     H**   PROGRAMMER: C:65                                          **
     H**   CREATED:    03/03/03     PROJECT: 129WAE                  **
     H**                                                             **
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/07/14 C:295 Project: 129WSB  Priss: 35                     *
     H*                Only compilation for the Changes made in the   *
     H*                printer file.                                  *
     H*                                                               *
     H* 11/22/13 C:190 Project 129WSB                                 *
     H*                Moved from client release(129) to base.        *
     H*                                                               *
     H* 10/20/06 C:65  PROJECT 129WAE/SUPP#55430                      *
     H*                NO LONGER PRINT THE NY POSTING NOTICE.         *
     H*                                                               *
     H* 11/06/03 C:61  PROJECT 129WEO                                 *
     H*                - Do not print cover letter if a renewal.      *
     H*                - Only print header info on first page of      *
     H*                  multi-page certificate holder list.          *
     H*                                                               *
     H* 11/05/03 C:65  PROJECT 129WAE/SUPP#39852                      *
     H*                ONLY PRINT POSTING NOTICES FOR ADDITIONAL      *
     H*                LOCATIONS THAT REPORT TO NCCI.                 *
     H*                                                               *
     H* 09/23/03 C:65  PROJECT 129WAE                                 *
     H*                PRINT A POSTING NOTICE FOR ADDITIONAL          *
     H*                LOCATIONS AS WELL.                             *
     H*                                                               *
     H* 05/21/03 C:61  PROJECT 129WAE                                 *
     H*                Print Claim Contact instead of Underwriter.    *
     H*                                                               *
     H*****************************************************************
     FWMFNL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWDELL1    IF   E           K DISK
     FWMECL1    IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMRPL     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWMCEL     IF   E           K DISK
     F*** BEGIN ADD ***  09/23/03  ************************************
     FWMALL     IF   E           K DISK
     F*** END ADD *****  09/23/03  ************************************
     FQSYSPRT2  O    F  125        PRINTER OFLIND(*INOF)
     F                                     USROPN
     FW7413O5C  O    E             PRINTER OFLIND(*IN01)
     F                                     USROPN
     F*** BEGIN DEL ***  10/20/06  *************************************
     F*****W7413O5NO   E             02     PRINTER                        UC
     F*** END DEL *****  10/20/06  *************************************
     FW7413O5M  O    E             PRINTER OFLIND(*IN03)
     F                                     USROPN

     D MNTH            S              9    DIM(12) CTDATA PERRCD(1)             NAME OF MNTH
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXDT                   1      8  0
     D  XXMTH                  1      2  0
     D  XXDAY                  3      4
     D  XXYR                   5      8
     D                 DS
     D  COPH1                  1      7  0
     D  XX1ST                  1      3
     D  XX2ND                  4      7
     D                 DS
     D  AGPHN1                 1      7  0
     D  XA1ST                  1      3
     D  XA2ND                  4      7
     D                 DS
     D  QQPOL#                 1     15
     D  QPFND                  1      3
     D  QPEMP#                 4     12
     D  QPFYR                 13     15
     D                 DS
     D  XXPFDT                 1      8  0
     D  XXPFYR                 5      8  0
     D*
     D XXEXT           C                   CONST('- Extension')

     C*
     C*  COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      *  Employer Contact
      *
     C     ECKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKPCNT            1
     C*
     C*   KEY LIST FOR THE CERTIFICATE MASTER FILE
     C*
     C     CEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XPFYR             3 0
     C                   KFLD                    XXEMP#
     C*
     C*  KEY LIST FOR EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*** BEGIN ADD ***  09/23/03  ************************************
     C*
     C*  ADDITIONAL LOCATIONS/ENTITIES FILE
     C*
     C     ALKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*** END ADD *****  09/23/03  ************************************
     C*
     C*  CARRIER/STATE REPORTING MASTER FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  KEY LIST FOR FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#            5 0
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
     C*
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQREN             1
     C                   PARM                    QQQUE            10
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQFND         QPFND
     C                   MOVE      QQEMP#        QPEMP#
     C                   MOVE      QQFYR         QPFYR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   Z-ADD     XXMTH         X                 2 0
     C                   MOVEL(P)  MNTH(X)       XXPRDT           20
     C                   CAT       XXDAY:1       XXPRDT
     C                   CAT       ',':0         XXPRDT
     C                   CAT       XXYR:1        XXPRDT
     C                   MOVE      '-'           XXDASH            1
     C*
     C     COKY          CHAIN     SMCOL                              60
     C  N60FNKY          CHAIN     PFN01                              60
     C  N60EMKY          CHAIN     PEM01                              60
     C  N60ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   EXSR      SRADR
     C                   EXSR      SRCNT
     C                   EXSR      SRSTF
     C                   EXSR      SRINFO
     C*
     C                   MOVEL(P)  '1-800-'      XXPHN#           14
     C                   CAT       XX1ST:0       XXPHN#
     C                   CAT       '-':0         XXPHN#
     C                   CAT       XX2ND:0       XXPHN#
     C*
      ***** BEG ADD ******* 11/06/03 **********************************
     C     QQREN         IFNE      'Y'
      ***** END ADD ******* 11/06/03 **********************************
     C                   OPEN      W7413O5C
     C                   WRITE     CVRLET
     C                   CLOSE     W7413O5C
      ***** BEG ADD ******* 11/06/03 **********************************
     C                   ENDIF
      ***** END ADD ******* 11/06/03 **********************************
     C*
     C* PRINT CERTIFICATE HOLDERS LIST IF RENEWAL
     C*
     C     QQREN         IFEQ      'Y'
     C*
     C     XXPFYR        SUB       1             XXPYR             4 0
     C     XXFYR         SUB       1             XPFYR
     C                   MOVE      *OFF          *IN61
     C     EMFDNM        IFNE      *BLANKS
     C                   MOVE      *ON           *IN61
     C                   ENDIF
     C*
     C                   OPEN      QSYSPRT2
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C*
     C* READ ALL CERTIFICATES FOR POLICY.
     C*
     C     CEKY          CHAIN     PCE01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     CESTAT        IFEQ      'A'
     C*
     C                   EXSR      SRCERT
      *
      ***** BEG DEL ******* 11/06/03 **********************************
     C*********  *INOF     IFEQ *ON
     C*********            EXCPTHEAD
     C*********            MOVE *OFF      *INOF
     C*********            ENDIF
      ***** END DEL ******* 11/06/03 **********************************
     C                   EXCEPT    CERT
     C*
     C                   ENDIF                                                  CESTAT IFEQ
     C*
     C     CEKY          READE     PCE01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   CLOSE     QSYSPRT2
     C*
     C                   ENDIF                                                  QQREN IFEQ
     C*
     C* CHECK ALL DIVISIONS FOR PRINTING POSTING NOTICES
     C*
     C     ELKY1         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      ELDIV         XXDIV
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVE      ELRTCD        XXRTCD
     C  N60RSKY          CHAIN     PRS01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   EXSR      SRINFO
     C*
     C* PRINT NY POSTING NOTICE
     C*
     C*** BEGIN DEL ***  10/20/06  *************************************
     C*********  RSJRST    IFEQ 'NY'
     C*********            OPEN W7413O5N
     C*********            WRITENYPST
     C*********            CLOSEW7413O5N
     C*********            ENDIF                           RSJRST IFEQ
     C*** END DEL *****  10/20/06  *************************************
     C*
     C* PRINT MA POSTING NOTICE
     C*
     C     RSJRST        IFEQ      'MA'
     C*
     C                   EXSR      SRFADR
     C                   EXSR      SRAGT
     C                   EXSR      SREADR
     C*
     C                   OPEN      W7413O5M
     C                   WRITE     MAPST
     C                   CLOSE     W7413O5M
     C                   ENDIF                                                  RSJRST IFEQ
     C*
     C*** BEGIN ADD ***  09/23/03  ************************************
     C     ALKY          CHAIN     PAL01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     ALSTAT        IFNE      'I'
     C*** BEGIN ADD ***  11/05/03  ************************************
     C     ALNCCI        ANDEQ     'Y'
     C*** END ADD *****  11/05/03  ************************************
     C*
     C* PRINT NY POSTING NOTICE
     C*
     C*** BEGIN DEL ***  10/20/06  *************************************
     C*********  RSJRST    IFEQ 'NY'
     C*********            OPEN W7413O5N
     C*********            WRITENYPST
     C*********            CLOSEW7413O5N
     C*********            ENDIF                           RSJRST IFEQ
     C*** END DEL *****  10/20/06  *************************************
     C*
     C* PRINT MA POSTING NOTICE
     C*
     C     RSJRST        IFEQ      'MA'
     C                   OPEN      W7413O5M
     C                   WRITE     MAPST
     C                   CLOSE     W7413O5M
     C                   ENDIF                                                  RSJRST IFEQ
     C*
     C                   ENDIF                                                  ALSTAT IFNE
     C*
     C     ALKY          READE     PAL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*** END ADD *****  09/23/03  ************************************
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     ELKY1         READE     PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   MOVE      '1'           *INLR
     C*****************************************************************
      *
      * Get POLICY INFO
      *
     C     SRINFO        BEGSR
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT            8 0
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPTDT            8 0
     C*
     C                   MOVE      *BLANKS       XXPOL#           20
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANKS
     C                   MOVEL     RPCAP#        XXPOL#
     C                   ELSE
     C                   MOVEL     QQPOL#        XXPOL#
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDSR                                                  SRINFO
     C*
     C*****************************************************************
      *
      * Get Contact Name
      *
     C     SRCNT         BEGSR
     C*
     C                   MOVE      *BLANKS       XXCNT
     C                   MOVE      *BLANKS       XXTITL
     C                   MOVE      '1'           KKPCNT
     C*
     C     ECKY          CHAIN     PEC01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     ECFSN         IFNE      *BLANKS
     C     ECFSN         CAT       ECLSN:1       XXCNT            50
     C                   ELSE
     C     ECTITL        CAT       ECLSN:1       XXCNT
     C                   ENDIF
     C     ECTITL        CAT       ECLSN:1       XXTITL           50
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDSR                                                  SRCNT
     C*****************************************************************
      *
      * GET STAFF NAMES
      *
     C     SRSTF         BEGSR
     C*
     C                   MOVE      *BLANKS       XXPOSR           60
      ***** BEG DEL ******* 05/21/03 **********************************
     C*********            MOVE *BLANKS   XXUNDE 60
      ***** END DEL ******* 05/21/03 **********************************
      ***** BEG ADD ******* 05/21/03 **********************************
     C                   MOVE      *BLANKS       XXCLMC           60
      ***** END ADD ******* 05/21/03 **********************************
     C                   MOVE      *BLANKS       XXLSCN           60
      ***** BEG DEL ******* 05/21/03 **********************************
     C*********            MOVE *BLANKS   @XUNDE 40
      ***** END DEL ******* 05/21/03 **********************************
      ***** BEG ADD ******* 05/21/03 **********************************
     C                   MOVE      *BLANKS       @XCLMC           40
      ***** END ADD ******* 05/21/03 **********************************
     C                   MOVE      *BLANKS       @XLSCN           40
      *
      *  GET POLICY SERVICES STAFF CODE, IF ENTERED
      *
     C     EMPSRV        IFNE      *BLANKS
     C     EMPSRV        CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     UPUSNM        XXPOSR
     C     UPEXT#        IFNE      *ZERO
     C                   CAT       XXEXT:1       XXPOSR
     C                   EXSR      SREXT
     C                   CAT       XXEXT#:1      XXPOSR
     C                   ENDIF                                                  UPEXT# IFNE
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  EMPSRV<>*BLANKS
      ***** BEG DEL ******* 05/21/03 **********************************
      *********
      ***GET*UNDERWRITER STAFF CODE, IF ENTERED
      *********
     C*********  EMUWTR    IFNE *BLANKS
     C*********  EMUWTR    CHAINPUP01                60
     C*********  *IN60     IFEQ *OFF
     C*********            MOVELUPUSNM    XXUNDE
     C*********            MOVELUPUSNM    @XUNDE
     C*********  UPEXT#    IFNE *ZERO
     C*********            CAT  XXEXT:1   XXUNDE
     C*********            EXSR SREXT
     C*********            CAT  XXEXT#:1  XXUNDE
     C*********            ENDIF                           UPEXT# IFNE
     C*********            ENDIF                           *IN60 IFEQ
     C*********            ENDIF                           EMUWTR<>*BLANKS
      ***** END DEL ******* 05/21/03 **********************************
      ***** BEG ADD ******* 05/21/03 **********************************
      *
      *  GET CLAIM CONTACT STAFF CODE, IF ENTERED
      *
     C     EMCLMC        IFNE      *BLANKS
     C     EMCLMC        CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     UPUSNM        XXCLMC
     C                   MOVEL     UPUSNM        @XCLMC
     C     UPEXT#        IFNE      *ZERO
     C                   CAT       XXEXT:1       XXCLMC
     C                   EXSR      SREXT
     C                   CAT       XXEXT#:1      XXCLMC
     C                   ENDIF                                                  UPEXT# IFNE
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  EMCLMC<>*BLANKS
      ***** END ADD ******* 05/21/03 **********************************
      *
      *  GET LOSS CONTROL REP STAFF CODE, IF ENTERED
      *
     C     EMLREP        IFNE      *BLANKS
     C     EMLREP        CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     UPUSNM        XXLSCN
     C                   MOVEL     UPUSNM        @XLSCN
     C     UPEXT#        IFNE      *ZERO
     C                   CAT       XXEXT:1       XXLSCN
     C                   EXSR      SREXT
     C                   CAT       XXEXT#:1      XXLSCN
     C                   ENDIF                                                  UPEXT# IFNE
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  EMLREP<>*BLANKS
      *
     C                   ENDSR                                                  SRCNT
     C*****************************************************************
      *
      * GET EMPLOYER ADDRESS
      *
     C     SRADR         BEGSR
     C*
     C                   MOVE      *BLANKS       XXADR1           40
     C                   MOVE      *BLANKS       XXADR2           40
     C                   MOVE      *BLANKS       XXCTYS           40
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVE      *BLANKS       XXZIP4
     C*
     C     EMMPFL        IFEQ      'M'
     C*
     C                   MOVEL     EMMAD1        XXADR1
     C     EMMAD2        IFNE      *BLANKS
     C                   MOVEL     EMMAD2        XXADR2
     C                   ENDIF                                                  EMMAD2 IFNE
     C*
     C                   ELSE
     C*
     C                   MOVEL     EMPAD1        XXADR1
     C     EMPAD2        IFNE      *BLANKS
     C                   MOVEL     EMPAD2        XXADR2
     C                   ENDIF                                                  EMPAD2 IFNE
     C*
     C                   ENDIF                                                  EMMPFL IFEQ
     C*
     C     EMMPFL        IFEQ      'M'
     C                   MOVEL     EMMCTY        XXLAST           56
     C                   MOVEL     EMMST         XXSTZP           13
     C                   MOVE      EMMZIP        XXSTZP
     C                   ELSE
     C                   MOVEL     EMPCTY        XXLAST           56
     C                   MOVEL     EMPST         XXSTZP           13
     C                   MOVE      EMPZIP        XXSTZP
     C                   ENDIF                                                  EMMPFL IFEQ
     C                   MOVE      XXSTZP        XXZIP4            4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5               5
     C     EMMPFL        IFEQ      'M'
     C                   MOVEL     EMMZIP        XXST10           10
     C                   ELSE
     C                   MOVEL     EMPZIP        XXST10           10
     C                   ENDIF
     C                   MOVE      XX5           XXST10           10
     C                   MOVE      XXST10        XXSTZP
     C                   ENDIF                                                  XXZIP4 IFEQ
     C*
     C                   MOVEL     XXSTZP        XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C*
     C     EMMPFL        IFEQ      'M'
     C*
     C     EMMAD2        IFNE      *BLANKS
     C                   MOVEL     XXLAST        XXCTYS
     C                   ELSE
     C                   MOVEL     XXLAST        XXADR2
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     EMPAD2        IFNE      *BLANKS
     C                   MOVEL     XXLAST        XXCTYS
     C                   ELSE
     C                   MOVEL     XXLAST        XXADR2
     C                   ENDIF
     C*
     C                   ENDIF                                                  EMMPFL IFEQ
     C*
     C                   ENDSR                                                  SRADR
     C*****************************************************************
      *
      * GET EMPLOYER ADDRESS VERSION 2
      *
     C     SREADR        BEGSR
     C*
     C                   MOVE      *BLANKS       XXEADR           60
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVE      *BLANKS       XXZIP4
     C*
     C     EMMPFL        IFEQ      'M'
     C*
     C                   MOVEL     EMMAD1        XXEADR
     C*
     C                   ELSE
     C*
     C                   MOVEL     EMPAD1        XXEADR
     C*
     C                   ENDIF                                                  EMMPFL IFEQ
     C*
     C     EMMPFL        IFEQ      'M'
     C                   MOVEL     EMMCTY        XXLAST           56
     C                   MOVEL     EMMST         XXSTZP           13
     C                   MOVE      EMMZIP        XXSTZP
     C                   ELSE
     C                   MOVEL     EMPCTY        XXLAST           56
     C                   MOVEL     EMPST         XXSTZP           13
     C                   MOVE      EMPZIP        XXSTZP
     C                   ENDIF                                                  EMMPFL IFEQ
     C                   MOVE      XXSTZP        XXZIP4            4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5               5
     C     EMMPFL        IFEQ      'M'
     C                   MOVEL     EMMZIP        XXST10           10
     C                   ELSE
     C                   MOVEL     EMPZIP        XXST10           10
     C                   ENDIF
     C                   MOVE      XX5           XXST10           10
     C                   MOVE      XXST10        XXSTZP
     C                   ENDIF                                                  XXZIP4 IFEQ
     C*
     C                   MOVEL     XXSTZP        XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C*
     C     EMMPFL        IFEQ      'M'
     C                   CAT       XXLAST:1      XXEADR
     C                   ENDIF                                                  EMMPFL IFEQ
     C*
     C                   ENDSR                                                  SREADR
     C*****************************************************************
     C*
     C*  BUILD EXTENSION FOR SERVICE TEAM NAME
     C*
     C     SREXT         BEGSR
     C*
     C                   MOVE      UPEXT#        XXEXT#            5
     C*
     C                   SELECT
     C     UPEXT#        WHENLT    10
     C                   MOVEL     '    '        XXEXT#
     C     UPEXT#        WHENLT    100
     C                   MOVEL     '   '         XXEXT#
     C     UPEXT#        WHENLT    1000
     C                   MOVEL     '  '          XXEXT#
     C     UPEXT#        WHENLT    10000
     C                   MOVEL     ' '           XXEXT#
     C                   ENDSL
     C*
     C                   ENDSR
     C******************************************************************
      *
      * GET FUND ADDRESS
      *
     C     SRFADR        BEGSR
     C*
     C                   MOVE      *BLANKS       XXFADR          100
     C                   MOVE      *BLANKS       XXTEMP           20
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVE      *BLANKS       XXZIP4
     C*
     C     FNMAD1        IFNE      *BLANKS
     C*
     C                   MOVEL     FNMAD1        XXFADR
     C     FNMAD2        IFNE      *BLANKS
     C                   MOVEL     FNMAD2        XXTEMP
     C                   CAT       XXTEMP:1      XXFADR
     C                   ENDIF                                                  EMMAD2 IFNE
     C*
     C                   ELSE
     C*
     C                   MOVEL     FNPAD1        XXFADR
     C     FNPAD2        IFNE      *BLANKS
     C                   MOVEL     FNPAD2        XXTEMP
     C                   CAT       XXTEMP:1      XXFADR
     C                   ENDIF                                                  EMPAD2 IFNE
     C*
     C                   ENDIF                                                  EMMPFL IFEQ
     C*
     C     FNMAD1        IFNE      *BLANKS
     C                   MOVEL     FNMCTY        XXLAST           56
     C                   MOVEL     FNMST         XXSTZP           13
     C                   MOVE      FNMZIP        XXSTZP
     C                   ELSE
     C                   MOVEL     FNPCTY        XXLAST           56
     C                   MOVEL     FNPST         XXSTZP           13
     C                   MOVE      FNPZIP        XXSTZP
     C                   ENDIF                                                  EMMPFL IFEQ
     C                   MOVE      XXSTZP        XXZIP4            4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5               5
     C     FNMAD1        IFNE      *BLANKS
     C                   MOVEL     FNMZIP        XXST10           10
     C                   ELSE
     C                   MOVEL     FNPZIP        XXST10           10
     C                   ENDIF
     C                   MOVE      XX5           XXST10           10
     C                   MOVE      XXST10        XXSTZP
     C                   ENDIF                                                  XXZIP4 IFEQ
     C*
     C                   MOVEL     XXSTZP        XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C*
     C                   CAT       XXLAST:1      XXFADR
     C*
     C                   ENDSR                                                  SRADR
     C******************************************************************
     C*
     C*  GET AGENT NAME VIA AGENT HISTORY AND AGENT MASTER FILES
     C*
     C     SRAGT         BEGSR
     C*
     C                   MOVE      *BLANKS       XXAGNM           30
     C                   MOVE      *BLANKS       XXAADR           60
     C                   MOVE      *BLANKS       XXAPHN           12
     C*
     C     AHKY          SETGT     PAH01
     C     AHKY          READPE    PAH01                                  60
     C     *IN60         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C     AHKY          READPE    PAH01                                  60
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVEL     AHAGT#        XXAGT#
     C     AAKY          CHAIN     PAA01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVEL     AAFEIN        XXFEIN
     C                   MOVEL     AAMOD         XXMOD
     C     AGKY          CHAIN     PAG01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVEL     AGNAME        XXAGNM
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVE      *BLANKS       XXZIP4
     C*
     C     AGMAD1        IFNE      *BLANKS
     C                   MOVEL     AGMAD1        AADD1            30
     C                   MOVEL     AGMCTY        XXSHRT           15
     C                   MOVEL     XXSHRT        XXLAST           56
     C                   MOVEL     AGMST         XXSTZP           13
     C                   MOVE      AGMZIP        XXSTZP
     C                   ELSE
     C                   MOVEL     AGPAD1        AADD1
     C                   MOVEL     AGPCTY        XXSHRT
     C                   MOVEL     XXSHRT        XXLAST
     C                   MOVEL     AGPST         XXSTZP
     C                   MOVE      AGPZIP        XXSTZP
     C                   ENDIF
     C*
     C                   MOVE      XXSTZP        XXZIP4            4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5               5
     C     AGMAD1        IFNE      *BLANKS
     C                   MOVEL     AGMZIP        XXST10           10
     C                   ELSE
     C                   MOVEL     AGPZIP        XXST10           10
     C                   ENDIF
     C                   MOVE      XX5           XXST10           10
     C                   MOVE      XXST10        XXSTZP
     C                   ENDIF
     C*
     C                   MOVEL     XXSTZP        XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C*
     C                   MOVEL     AADD1         XXAADR
     C                   CAT       XXLAST:1      XXAADR
     C*
     C     AGPHN1        IFNE      *ZERO
     C                   MOVEL     AGARE1        XXAPHN
     C                   CAT       '-':0         XXAPHN
     C                   CAT       XA1ST:0       XXAPHN
     C                   CAT       '-':0         XXAPHN
     C                   CAT       XA2ND:0       XXAPHN
     C                   ENDIF                                                  AGPHN1 IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDSR                                                  SRAGT
     C******************************************************************
     C*
     C* CERTIFICATE HOLDERS INFO.
     C*
     C     SRCERT        BEGSR
     C*
     C                   MOVEA     '00'          *IN(62)
     C*
     C     CEATTN        IFNE      *BLANKS
     C                   MOVE      *ON           *IN62
     C                   ENDIF
     C*
     C     CEADR2        IFNE      *BLANKS
     C                   MOVE      *ON           *IN63
     C                   ENDIF
     C*
     C*   COMBINE CITY STATE AND ZIP
     C*
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVE      *BLANKS       XXZIP5                                     8
     C                   MOVE      *BLANKS       XXZIP4
     C                   MOVEL     CECTY         XXLAST           56
     C                   MOVEL     CEST          XXSTZP           13
     C                   MOVEL     CEZIP         XXZIP5            5                        8
     C                   MOVE      CEZIP         XXSTZP
     C                   MOVE      XXSTZP        XXZIP4            4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5               5
     C                   MOVEL     XXZIP5        XXST10                                     8
     C                   MOVE      XX5           XXST10           10
     C                   MOVE      XXST10        XXSTZP
     C                   END
     C*
     C*  CONCATENATE CITY, STATE AND ZIP
     C*
     C                   MOVEL     XXSTZP        XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        CECSZ            40
     C*
     C                   ENDSR                                                  SRCERT
     C******************************************************************
     OQSYSPRT2  E            HEAD           2 06
     O                                           21 'Member Name:'
     O              N61      EMNAME              69
     O               61      EMFDNM              69
     O          E    61      HEAD           2
     O                                           13 'DBA:'
     O                       EMNAME              69
     O          E            HEAD           2
     O                                           28 'Certificate Number:'
     O                       XXPOL#              49
     O          E            HEAD           3
     O                                           24 'Effective Date:'
     O                       XXPFDT              39 '  /  /    '
     O          E            HEAD           1
     O                                           32 'The following is a list'
     O                                           56 'of Certificates of Insu'
     O                                           79 'rance that have been is'
     O                                          102 'sued on your behalf dur'
     O                                          109 'ing the'
     O          E            HEAD           1
     O                       XXPYR               13
     O                                           37 'certificate period.  Pl'
     O                                           60 'ease contact your Custo'
     O                                           83 'mer Service Representat'
     O                                          106 'ive should you need any'
     O          E            HEAD           2
     O                                           32 'of these Certificates i'
     O                                           46 'ssued for your'
     O                       XXPFYR              51
     O                                           74 'Workers'' Compensation '
     O                                           82 'renewal.'
     O          E            HEAD           1
     O                                           32 'There is a form on the '
     O                                           55 'back of your renewal ja'
     O                                           78 'cket that you may compl'
     O                                          101 'ete and fax to us for a'
     O                                          107 'ny new'
     O          E            HEAD           1    22
     O                                           32 'Certificate of Insuranc'
     O                                           43 'e requests.'
     O          E            CERT           1
     O                       CENAME              54
     O          E    62      CERT           1
     O                                           24 'Attention:'
     O                       CEATTN              85
     O          E            CERT           1
     O                       CEADR1              54
     O          E    63      CERT           1
     O                       CEADR2              54
     O          E            CERT           2
     O                       CECSZ               54
**
January
February
March
April
May
June
July
August
September
October
November
December
