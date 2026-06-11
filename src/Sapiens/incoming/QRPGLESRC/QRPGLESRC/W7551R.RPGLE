     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7551R - ENTITY/LOCATION EMPLOYER LABELS
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  THIS PROGRAM PRINTS ALL OR ONE OF THESE LABEL TYPES FOR A    *
     H*  SELECTION OF ENTITIES/LOCAITONS: REGULAR, LOGO, AND POSTER.  *
     H*                                                               *
     H*  CALLED BY: W7551C - OVERRIDE CL                              *
     H*                                                               *
     H*  CALLS TO:  NONE                                              *
     H*                                                               *
     H*  PROGRAMMER: C:24                                             *
     H*  CREATED:    11/01/94          PROJECT: WC378                 *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/25/99  C:65  PROJECT: WC908                                *
     H*                 PULL STANDARD INCR LIMITS FROM WDRRP INSTEAD  *
     H*                 OF WDF1P.                                     *
     H*                                                               *
     H* 10/29/98  C:35   PROJECT: SUPPORT                             *
     H*                  USE THE MOST RECENT FYR FOR EACH EMPLOYER    *
     H*                  INSTEAD OF THE FYR FROM USER PARAMETERS.     *
     H*                                                               *
     H* 09/03/98  C:90  PROJECT: WC577                                *
     H*                 LABEL PRINT WILL USE THE ALTERNATE NAME AND   *
     H*                 ADDRESS IF AVAILABLE.                         *
     H*                                                               *
     H* 06/16/98  C:90  PROJECT: WC705                                *
     H*                 CHECK FOR INACTIVE INCR LIMIT CODES.          *
     H*                                                               *
     H*  03/15/95 C:24 PROJECT WC405                                  *
     H*                ADD FUND TO INCREASED LIMITS TABLE KEY.        *
     H*                                                               *
     H*  08/12/96 C:60 PROJECT WC543                                  *
     H*                ADD AN IF STATEMENT TO CHECK IF ENTTITY/       *
     H*                LOCATION HAS A PHYSICAL ADDRESS. ONLY PRINT    *
     H*                THE MAILING ADDRESS IF THE PHYSICAL IS BLANK   *
     H*                                                               *
     H*  06/13/97 C:62 PROJECT WC681                                  *
     H*                NO MORE HARD CODED 800# USING FNPH1A           *
     H*                                                               *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*  WMUPL   PUP01: USER                                USER PARM *
     H*  SMCOL   PCO01: CO#                                   COMPANY *
     H*  WMFNL   PFN01: CO#/FND                                  FUND *
     H*  WDF1L   PF101: CO#/FND/FYR                       FUND DETAIL *
     H*  WMEML   PEM01: CO#/FND/EMP#/DIV              EMPLOYER MASTER *
     H*  WMALL   PAL01: CO#/FND/EMP#/DIV/ELID              ENTITY/LOC *
     H*  WMAHL   PAH01: CO#/FND/EMP#/DIV         AGENT HISTORY MASTER *
     H*  WDELL   PEL01: CO#/FND/EMP#/DIV/FYR          EMPLOYER DETAIL *
     H*  WTLML   PLM01: CO#/KEY                      LIABILITY LIMITS *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*   21  ALL EMPLOYERS                                           *
     H*   22  ALL ENTITIES/LOCATIONS                                  *
     H*   23  ALL LABELS REQUESTED                                    *
     H*   24  REGULAR EMPLOYER LABELS REQUESTED                       *
     H*   25  LOGO EMPLOYER LABELS REQUESTED                          *
     H*   26  POSTER LABELS REQUESTED                                 *
     H*   31  E/L'S SECOND ADDRESS LINE IS NOT BLANK                  *
     H*   32  LIMITS DESCRIPTION IS NOT BLANK                         *
     H*   60  GENERAL READS/CHAINS                                    *
     H*   61  READS/CHAINS OVER EMPLOYER MASTER FILE                  *
     H*   91  PGM IS CALLED FROM ENTITY/LOCATION MAINTENANCE PGM.     *
     H*       DON'T ALLOW COMMAND-13; ALLOW CMD-2.                    *
     H*  N91  PGM IS CALLED FROM MENU. ALLOW COMMAND 13. NO CMD-2.    *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*****************************************************************
     F*
     FWMUPL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F*** BEGIN CHG ***  10/25/99  ************************************
     F*****WDF1L   IF  E           K        DISK
     FWDRRL     IF   E           K DISK
     F*** END CHG *****  10/25/99  ************************************
     FWMEML     IF   E           K DISK
     FWMALL     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWTLML1    IF   E           K DISK
     F*
     F* * * OUTPUT FILES
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     F                                     USROPN
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOF)
     F                                     USROPN
     FQSYSPRT2  O    F  198        PRINTER OFLIND(*INOB)
     F                                     USROPN
     F*

     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D**** BEGIN ADD **** 09/03/98 ************************************
     D**** END   ADD **** 09/03/98 ************************************
     D*
     D* System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND                       COMPANY NAME
     D  CONAME                 1     40
     D**** BEGIN ADD **** 09/03/98 ************************************
     D                 DS
     D  ANAM                   1     50
     D                                     DIM(50) ASCEND                       ALTERNATE CO NAME
     D  FNANAM                 1     50
     D**** END   ADD **** 09/03/98 ************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   FUND DETAIL FILE
     C*
     C*** BEGIN CHG ***  10/25/99  ************************************
     C********   F1KY      KLIST
     C********             KFLD           XXCO#
     C********             KFLD           XXFND
     C********             KFLD           XXFYR
      *
      *   STATISTICAL REPORTING/RATING FILE
      *
     C     RRKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXSTA
     C                   KFLD                    ELPFDT
      *
     C     RRKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXSTA
     C*** END CHG *****  10/25/99  ************************************
     C*
     C*  KEY LIST FOR EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  KEY LIST FOR ENTITY/LOCATION MASTER FILE
     C*
     C     ALKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     ALKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXELID
     C*
     C*  KEY LIST FOR AGENT HISTORY MASTER FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
      **** BEGIN ADD **** 10/29/98 *********************************************
      *
      * KEY LIST FOR THE EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
      **** END   ADD **** 10/29/98 *********************************************
     C*
     C*  KEY LIST FOR RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
     C*
     C*  KEY LIST FOR INCR LIMITS TABLE FILE
     C*
     C     LMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXSTA             2
     C                   KFLD                    XXKEY            10
     C                   KFLD                    XXEFDT
     C*
     C     *LIKE         DEFINE    ALCO#         XXCO#
     C     *LIKE         DEFINE    ALFND         XXFND
     C     *LIKE         DEFINE    ALEMP#        XXEMP#
     C     *LIKE         DEFINE    ALDIV         XXDIV
     C     *LIKE         DEFINE    ALELID        XXELID
     C     *LIKE         DEFINE    ALDT          XXEFDT
     C     *LIKE         DEFINE    AHAGT#        XXAGT#
     C     *LIKE         DEFINE    AHFYR         XXFYR
     C     *LIKE         DEFINE    FNAREA        XXAREA
     C     *LIKE         DEFINE    FNPH1         XXPHON
     C*** BEGIN CHG ***  10/25/99  ************************************
     C*********  *LIKE     DEFN F1EMLB    XXEMLB
     C     *LIKE         DEFINE    RREMLB        XXEMLB
     C*** END CHG *****  10/25/99  ************************************
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQELID           10            ENTITY/LOCATION ID#
     C                   PARM                    QQLBTY            1            LABEL TYPE
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
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQELID        XXELID
     C                   MOVE      QQELID        XXEL#            10 0
     C                   TIME                    DSDTTM
     C                   MOVE      ''''          XXQTE             1            AN APOSTROPHE
     C                   MOVE      'N'           XXOPN1            1
     C                   MOVE      'N'           XXOPN2            1
     C                   MOVE      'N'           XXOPN3            1
     C     Q1USER        CHAIN     PUP01                              60        GET USER INFO
      **** BEGIN DEL **** 10/29/98 *********************************************
     C******************** Z-ADDUPFYR     XXFYR
      **** END   DEL **** 10/29/98 *********************************************
     C*
     C*  SET INDICATORS FOR REPORT SELECTIONS
     C*
     C                   MOVEA     '000000'      *IN(21)
     C     XXEMP#        IFEQ      *ZERO
     C     XXDIV         ANDEQ     *ZERO
     C                   MOVE      '1'           *IN21                           ALL EMPLOYERS
     C                   END
     C     XXEL#         COMP      *ZERO                                  22     ALL ENTITIES/LOCS
     C     QQLBTY        COMP      *BLANK                                 23     ALL LABEL TYPES
     C     QQLBTY        COMP      'E'                                    24     REGULAR EMP LABELS
     C     QQLBTY        COMP      'L'                                    25     LOGO EMP LABELS
     C     QQLBTY        COMP      'P'                                    26     POSTER LABELS
     C*
     C*  OPEN NECESSARY SPOOL FILES
     C*
     C     XXOPN1        IFEQ      'N'
     C     QQLBTY        IFEQ      'E'
     C     QQLBTY        OREQ      ' '
     C                   OPEN      QSYSPRT
     C                   MOVE      'Y'           XXOPN1
     C                   END
     C                   END
     C*
     C     XXOPN2        IFEQ      'N'
     C     QQLBTY        IFEQ      'L'
     C     QQLBTY        OREQ      ' '
     C                   OPEN      QSYSPRT1
     C                   MOVE      'Y'           XXOPN2
     C                   END
     C                   END
     C*
     C     XXOPN3        IFEQ      'N'
     C     QQLBTY        IFEQ      'P'
     C     QQLBTY        OREQ      ' '
     C                   OPEN      QSYSPRT2
     C                   MOVE      'Y'           XXOPN3
     C                   END
     C                   END
     C*
     C*  PRINT EMPLOYER AND/OR POSTER LABELS FOR EACH EMPLOYER
     C*
     C   21EMKY          CHAIN     PEM01                              61        ALL EMPLOYERS
     C  N21EMKY1         CHAIN     PEM01                              61        ONE EMP
     C     *IN61         DOWEQ     '0'
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     EMDIV         XXDIV
     C*
     C*  GET COMPANY, FUND, AND POLICY INFO OR POSTER LABELS
     C*  (SAME FOR EACH ENTITY/LOCATION WTIHIN THIS EMPLOYER).
     C*
     C     *IN23         IFEQ      '1'
     C     *IN26         OREQ      '1'
     C                   EXSR      SRINFO
     C                   END
     C*
     C*  PROCESS EACH ENTITY/LOCATION WITHIN THIS EMPLOYER
     C*
     C   22ALKY          CHAIN     PAL01                              60        ALL ENTITIES/LOCS
     C  N22ALKY1         CHAIN     PAL01                              60        ONE ENTITY/LOC
     C     *IN60         DOWEQ     '0'
     C     ALDLDT        IFEQ      0
     C                   MOVE      ALELID        XXELID
     C*
     C*  GET ENTITY/LOCATION NAME & ADDRESS
     C*
     C                   MOVE      *BLANK        XXDBA            33            E/L NAME
     C                   MOVE      *BLANK        XXADR1           33
     C                   MOVE      *BLANK        XXADR2           33
     C                   MOVE      *BLANK        XXCSZ            33            CITY, STATE, ZIP
     C                   MOVEL     ALNAME        XXNAME           33
     C                   MOVEL     ALMAD1        XXADR1
     C     ALPAD1        IFNE      *BLANK
     C                   MOVEL     ALPAD1        XXADR1
     C     ALPAD2        COMP      *BLANK                             3131        HAVE 2ND ADDRESS L
     C   31              MOVEL     ALPAD2        XXADR2
     C*
     C     ALPCTY        CAT       ',':0         XXCSZ
     C     XXCSZ         CAT       ALPST:1       XXCSZ
     C                   MOVEL     ALPZIP        XXZIP1            5
     C                   MOVE      ALPZIP        XXZIP2            4
     C     XXCSZ         CAT       XXZIP1:1      XXCSZ
     C     XXZIP2        IFNE      *ZEROS
     C     XXCSZ         CAT       '-':0         XXCSZ
     C     XXCSZ         CAT       XXZIP2:0      XXCSZ
     C                   END
     C                   ELSE
     C                   MOVEL     ALMAD1        XXADR1
     C     ALMAD2        COMP      *BLANK                             3131        HAVE 2ND ADDRESS L
     C   31              MOVEL     ALMAD2        XXADR2
     C*
     C     ALMCTY        CAT       ',':0         XXCSZ
     C     XXCSZ         CAT       ALMST:1       XXCSZ
     C                   MOVEL     ALMZIP        XXZIP1            5
     C                   MOVE      ALMZIP        XXZIP2            4
     C     XXCSZ         CAT       XXZIP1:1      XXCSZ
     C     XXZIP2        IFNE      *ZEROS
     C     XXCSZ         CAT       '-':0         XXCSZ
     C     XXCSZ         CAT       XXZIP2:0      XXCSZ
     C                   END
     C*
     C                   ENDIF
     C     *IN24         IFEQ      '1'                                          PRINT REGULAR LABELS
     C     *IN23         OREQ      '1'
     C                   EXCEPT    REG
     C                   END                                                    IF *24
     C*
     C     *IN25         IFEQ      '1'                                          PRINT LOGO LABELS
     C     *IN23         OREQ      '1'
     C                   EXSR      SRAGT                                        GET AGENT #
     C                   EXCEPT    LOGO
     C                   END                                                    IF *25
     C*
     C     *IN26         IFEQ      '1'                                          PRINT POSTER LABELS
     C     *IN23         OREQ      '1'
     C*
     C*  PUT COMPLETE ADDRESS OF E/L ON ONE LINE
     C*
     C                   MOVE      *BLANK        XXADDR           65            COMPLETE ADDRESS
     C     XXADR1        CAT       ',':0         XXADDR
     C     XXADDR        CAT       XXCSZ:1       XXADDR
     C                   EXCEPT    POSTER
     C                   END                                                    IF *24
     C                   ENDIF                                                  ALDLDT IFEQ 0
     C*
     C   22ALKY          READE     PAL01                                  60     ALL ENTS/LOCS
     C  N22              MOVE      '1'           *IN60                           ONE ENT/LOC
     C                   END                                                    DOW *60, ALKY
     C*
     C   21EMKY          READE     PEM01                                  61     ALL EMPLOYERS
     C  N21              MOVE      '1'           *IN61                           ONE EMP
     C                   END                                                    DOW *61, EMKY
     C*
     C*  END OF PROGRAM
     C*
     C*  CLOSE NECESSARY SPOOL FILES
     C*
     C     XXOPN1        IFEQ      'Y'
     C     QQLBTY        IFEQ      'E'
     C     QQLBTY        OREQ      ' '
     C                   CLOSE     QSYSPRT
     C                   MOVE      'Y'           XXOPN1
     C                   END
     C                   END
     C*
     C     XXOPN2        IFEQ      'Y'
     C     QQLBTY        IFEQ      'L'
     C     QQLBTY        OREQ      ' '
     C                   CLOSE     QSYSPRT1
     C                   MOVE      'Y'           XXOPN2
     C                   END
     C                   END
     C*
     C     XXOPN3        IFEQ      'Y'
     C     QQLBTY        IFEQ      'P'
     C     QQLBTY        OREQ      ' '
     C                   CLOSE     QSYSPRT2
     C                   MOVE      'Y'           XXOPN3
     C                   END
     C                   END
     C                   MOVE      '1'           *INLR
     C******************************************************
     C*  SRAGT - SUBROUTINE TO GET THE AGENT NUMBER        *
     C******************************************************
     CSR   SRAGT         BEGSR
     C*
     C     AHKY          SETGT     PAH01
     C                   READP     PAH01                                  60
     C     *IN60         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  60
     C                   END
     C     *IN60         IFEQ      '0'
     C     AHCO#         ANDEQ     XXCO#
     C     AHFND         ANDEQ     XXFND
     C     AHEMP#        ANDEQ     EMEMP#
     C     AHDIV         ANDEQ     EMDIV
     C     AHFYR         ANDEQ     XXFYR
     C                   Z-ADD     AHAGT#        XXAGT#
     C                   ELSE
     C                   Z-ADD     *ZERO         XXAGT#
     C                   END
     C                   ENDSR
     C******************************************************
     C*  SRINFO- SUBROUTINE TO GET INFO FOR POSTER LABEL   *
     C******************************************************
     CSR   SRINFO        BEGSR
     C*
     C*  GET THE COMPANY NAME AND ADDRESS
     C*
     C     COKY          CHAIN     PCO01                              60
     C**** BEGIN ADD **** 09/03/98 ************************************
     C     FNKY          CHAIN     PFN01                              60
     C**** END   ADD **** 09/03/98 ************************************
     C*
     C*  REMOVE LEADING BLANKS FROM COMPANY NAME
     C*
     C**** BEGIN ADD **** 09/03/98 ************************************
     C     FNANAM        IFNE      *BLANKS
     C                   Z-ADD     1             X                 2 0
     C     *BLANK        LOOKUP    ANAM(X)                            60
     C                   MOVEA     ANAM(X)       XXCONM           33
     C                   ELSE
     C**** END   ADD **** 09/03/98 ************************************
     C                   Z-ADD     1             X                 2 0
     C     *BLANK        LOOKUP    CONM(X)                            60
     C                   MOVEA     CONM(X)       XXCONM           33
     C**** BEGIN ADD **** 09/03/98 ************************************
     C                   ENDIF                                                  ANAM NE BLANKS
     C**** END   ADD **** 09/03/98 ************************************
     C*
     C*  LOAD COMPLETE ADDRESS OF COMPANY ON ONE LINE
     C*
     C**** BEGIN ADD **** 09/03/98 ************************************
     C     FNAAD1        IFNE      *BLANKS
     C                   MOVE      *BLANK        XXCCSZ           33
     C     FNACTY        CAT       ',':0         XXCCSZ
     C     XXCCSZ        CAT       FNAST:1       XXCCSZ
     C                   MOVEL     FNAZIP        XXZIP1            5
     C                   MOVE      FNAZIP        XXZIP2            4
     C     XXCCSZ        CAT       XXZIP1:1      XXCCSZ
     C     XXZIP2        IFNE      *ZEROS
     C     XXCCSZ        CAT       '-':0         XXCCSZ
     C     XXCCSZ        CAT       XXZIP2:0      XXCCSZ
     C                   END
     C                   MOVE      *BLANK        XXCADR           65            CO'S COMPLETE ADDRES
     C                   MOVE      *BLANK        XXCAD1           33
     C                   MOVEL     FNAAD1        XXCAD1
     C     FNAAD2        IFNE      *BLANKS
     C     XXCAD1        CAT       ',':0         XXCAD1
     C     XXCAD1        CAT       FNAAD2:1      XXCAD1
     C                   ENDIF                                                  AAD2 NE BLANKS
     C     XXCAD1        CAT       ',':0         XXCADR
     C     XXCADR        CAT       XXCCSZ:1      XXCADR
     C                   ELSE
     C**** END   ADD **** 09/03/98 ************************************
     C                   MOVE      *BLANK        XXCCSZ           33
     C     COCTY         CAT       ',':0         XXCCSZ
     C     XXCCSZ        CAT       COST:1        XXCCSZ
     C                   MOVEL     COZIP         XXZIP1            5
     C                   MOVE      COZIP         XXZIP2            4
     C     XXCCSZ        CAT       XXZIP1:1      XXCCSZ
     C     XXZIP2        IFNE      *ZEROS
     C     XXCCSZ        CAT       '-':0         XXCCSZ
     C     XXCCSZ        CAT       XXZIP2:0      XXCCSZ
     C                   END
     C                   MOVE      *BLANK        XXCADR           65            CO'S COMPLETE ADDRES
     C                   MOVE      *BLANK        XXCAD1           33
     C**** BEGIN CHG **** 09/03/98 ************************************
     C******************** MOVELCOADR2    XXCAD1
     C                   MOVEL     COADR1        XXCAD1
     C     COADR2        IFNE      *BLANKS
     C     XXCAD1        CAT       ',':0         XXCAD1
     C     XXCAD1        CAT       COADR2:1      XXCAD1
     C                   ENDIF                                                  ARD2 NE BLANKS
     C**** END   CHG **** 09/03/98 ************************************
     C     XXCAD1        CAT       ',':0         XXCADR
     C     XXCADR        CAT       XXCCSZ:1      XXCADR
     C**** BEGIN ADD **** 09/03/98 ************************************
     C                   ENDIF                                                  AAD1 NE BLANKS
     C**** END   ADD **** 09/03/98 ************************************
     C*
     C*  GET THE FUND NAME AND PHONE NUMBER
     C*
     C**** BEGIN DEL **** 09/03/98 ************************************
     C********** FNKY      CHAINPFN01                60
     C**** END   DEL **** 09/03/98 ************************************
     C     FNPH1         IFNE      *ZERO
     C*** BEGIN DEL *** 06/13/97 **************************************
     C**********           Z-ADD800       XXAREA
     C*** END   DEL *** 06/13/97 **************************************
     C*** BEGIN ADD *** 06/13/97 **************************************
     C     FNPH1A        IFNE      *ZERO
     C                   Z-ADD     FNPH1A        XXAREA
     C                   ELSE
     C                   Z-ADD     800           XXAREA
     C                   ENDIF
     C*** END   ADD *** 06/13/97 **************************************
     C                   Z-ADD     FNPH1         XXPHON
     C                   ELSE
     C                   Z-ADD     FNAREA        XXAREA
     C                   Z-ADD     FNPH2         XXPHON
     C                   END
     C                   MOVEL     FNSHNM        XXSHNM           27
      **** BEGIN DEL **** 10/29/98 *********************************************
     C**********
     C** GET THE EMPLOYER LIABILITY LIMIT AMOUNT FROM FUND DETAIL FILE
     C**********
     C********** F1KY      CHAINPF101                60
     C**********           MOVE *BLANKS   XXEMLB
     C******* N60          MOVELF1EMLB    XXEMLB           DEFAULT LIAB LIMITS
      **** END   DEL **** 10/29/98 *********************************************
     C*
     C*  GET THE POLICY FROM AND TO DATES FROM EMPLOYER DETAIL FILE
     C*
     C                   Z-ADD     *ZERO         XXPFDT
     C                   Z-ADD     *ZERO         XXPTDT
      **** BEGIN CHG **** 10/29/98 *********************************************
     C********** AHKY      CHAINPEL01                60
     C     ELKY          SETGT     PEL01
     C     ELKY          READPE    PEL01                                  60
      **** END   CHG **** 10/29/98 *********************************************
     C     *IN60         IFEQ      '0'
      **** BEGIN ADD **** 10/29/98 *********************************************
     C                   Z-ADD     ELFYR         XXFYR
     C*
     C*  GET THE EMPLOYER LIABILITY LIMIT AMOUNT FROM FUND DETAIL FILE
     C*
     C*** BEGIN DEL ***  10/25/99  ************************************
     C*********  F1KY      CHAINPF101                60
     C*** END DEL *****  10/25/99  ************************************
     C                   MOVE      *BLANKS       XXEMLB
     C*** BEGIN DEL ***  10/25/99  ************************************
     C**N60***********     MOVELF1EMLB    XXEMLB           DEFAULT LIAB LIMITS
     C*** END DEL *****  10/25/99  ************************************
      **** END   ADD **** 10/29/98 *********************************************
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPFDT            6 0
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPTDT            6 0
     C*
     C*  GET DESCRIPTION FOR INCREASE LIMITS IF OVERRIDDEN
     C*
     C     ELILCD        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     ELILCD        XXKEY
     C     ELILEF        IFEQ      *ZERO
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   ELSE
     C                   Z-ADD     ELILEF        XXEFDT
     C                   END
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXSTA
     C*
     C     LMKY          SETGT     PLM01                              60
     C                   READP     PLM01                                  60
     C*** BEGIN ADD ***  06/16/98  ************************************
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C                   READP     PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*** END ADD *****  06/16/98  ************************************
     C  N60XXCO#         COMP      LMCO#                              6060
     C  N60XXFND         COMP      LMFND                              6060
     C  N60XXSTA         COMP      LMST                               6060
     C  N60XXKEY         COMP      LMKEY                              6060
     C  N60ELPFDT        COMP      LMEFDT                               60
     C  N60              MOVEL     LMDESC        XXEMLB
     C                   END
     C*** BEGIN ADD ***  10/25/99  ************************************
     C     XXEMLB        IFEQ      *BLANKS
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXSTA
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     RREMLB        XXEMLB
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  XXEMLB IFEQ
     C*** END ADD *****  10/25/99  ************************************
     C     XXEMLB        COMP      *BLANKS                            3232
     C                   END                                                    IF *60, ELKY
     C                   ENDSR
     O*****************************************************************
     O* Regular Labels
     O*
     OQSYSPRT   E            REG            1 01
     O                       EMFND               04
     O                                           05 '-'
     O                       EMEMP#              14
     O                       EMDIV         Z     20
     O                                           30 'E/L #:'
     O                       XXELID              41
     O          E            REG            1
     O                       XXNAME              34
     O          E            REG            1
     O                       XXADR1              34
     O          E    31      REG            1
     O                       XXADR2              34
     O          E            REG            1
     O                       XXCSZ               34
     O*
     O* Logo Labels
     O*
     OQSYSPRT1  E            LOGO           1 11
     O                       EMFND               13
     O                                           14 '-'
     O                       EMEMP#              23
     O                       EMDIV         Z     29
     O                                           38 'AGENT#'
     O                       XXAGT#        Z     44
     O          E            LOGO           1
     O                                           21 'E/L NUMBER:'
     O                       XXELID              32
     O          E            LOGO           1
     O                       XXNAME              38
     O          E            LOGO           1
     O                       XXADR1              38
     O          E    31      LOGO           1
     O                       XXADR2              38
     O          E            LOGO           1
     O                       XXCSZ               38
     O*
     O*  POSTER LABELS
     O*
     OQSYSPRT2  E            POSTER         1 02
     O                                            9 'Employer:'
     O                       ALNAME              50
     O          E            POSTER         1
     O                       XXADDR              65
     O          E            POSTER         1
     O                                           19 'is providing Worker'
     O                       XXQTE               20
     O                                           42 's Compensation through'
     O          E            POSTER         2
     O                       XXSHNM              32
     O                                           48 'Effective date:'
     O                       XXPFDT        Y     57
     O                                           58 '-'
     O                       XXPTDT        Y     66
     O          E            POSTER         1
     O                                           14 'Policy Number:'
     O                       XXFND               19
     O                                           20 '-'
     O                       EMEMP#              29
     O                       EMDIV         Z     36
     O                                           50 'E/L Number:'
     O                       XXELID              61
     O          E            POSTER         1
     O                                           20 'Local representative'
     O                                           30 'or agent:'
     O          E            POSTER         1
     O                       XXCONM              55
     O          E            POSTER         1
     O                       XXCADR              65
     O          E            POSTER         1
     O               32                           8 'Employer'
     O               32      XXQTE                9
     O               32                          21 's Liability:'
     O               32      XXEMLB              52
     O                                           54 '('
     O                       XXAREA              57
     O                                           58 ')'
     O                       XXPHON              67 '   -    '
