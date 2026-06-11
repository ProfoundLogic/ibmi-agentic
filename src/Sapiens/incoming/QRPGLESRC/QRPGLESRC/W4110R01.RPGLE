     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W4110R01 - SAMPLE ADDITIONAL INPUT PROGRAM
     H*****************************************************************
     H* PROGRAM DESCRIPTION                                           *
     H*                                                               *
     H* This program prompts the user for additional input necessary  *
     H* to print a department letter.                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     W4110R                                                    *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*     NONE                                                      *
     H*                                                               *
     H*  PROGRAMMER: C:09                                             *
     H*                                                               *
     H*  CREATED:    03/22/95               PROJECT: WC376            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H* 04/12/99 C:90 PROJECT: WC889                                  *
     H*               IF DBA FLAG = 'Y' LOAD PDADR2, ELSE LOAD PDADR1 *
     H*                                                               *
     H*****************************************************************
     FW4110F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDCLL     UF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMDEL     IF   E           K DISK
     FWMDOL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMPDL     IF   E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     FWDC2P     O    E             DISK

     D********            ST3    26  52  2
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D*   ADDITIONAL PROGRAM PARMS
     D                 DS
     D  PARMS                  1    136
     D  QQCO#                  1      3
     D  QQFND                  4      6
     D  QQREQ#                 7     11
     D  QQDPCD                12     16
     D  QQDCID                17     28
     D  QQDESC                29     56
     D  QQFLDR                57    136
     D                 DS
     D  XXCZIP                 1      9  0
     D  SC4ZP1                 1      5  0
     D  SC4ZP2                 6      9  0
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D                 DS
     D  CL20A2                 1     20
     D  XXDARA                 1      3
     D  XXDPH1                 4      6
     D  XXDPH2                 7     10
     D  XXMISC                11     20
     D OPTNM           C                   CONST('Additional Input -
     D                                     Screen')
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR CLAIMANT DEMOGRAPHICS
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY FOR LETTER WORKFILE
     C*
     C     CLKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXREQ#
     C                   KFLD                    QQDPCD
     C                   KFLD                    QQFLDR
     C                   KFLD                    QQDCID
     C*
     C*  CLAIM MASTER KEY LIST
     C*
     C     CMKEY         KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLFYR
     C                   KFLD                    CLFCLM
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  DEPT MASTER FILE
     C*
     C     DEKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    QQDPCD
     C*
     C*  DEPT/DOC ID MASTER FILE
     C*
     C     DOKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    QQDPCD
     C                   KFLD                    QQFLDR
     C                   KFLD                    QQDCID
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  STATE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    STKEY         XXKEY
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    CLFND         XXFND
     C     *LIKE         DEFINE    CLREQ#        XXREQ#
     C     *LIKE         DEFINE    DEDEPT        XXDEPT
     C     *LIKE         DEFINE    PDFEIN        XXFEIN
     C     *LIKE         DEFINE    PDMOD         XXMOD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQPRMS          136
     C                   PARM                    XXDEPT
     C                   PARM                    QQRTCD            1
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
     C                   MOVE      QQPRMS        PARMS
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQREQ#        XXREQ#
     C                   MOVE      *BLANK        QQRTCD
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE FUND RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C******               MOVE QQPRDS    SCPRDS
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C*  GET THE DEPT LETTER WORKFILE RECORD
     C*
     C     CLKEY         CHAIN     PCL01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      *ZEROS        SCFEN3                         SCH DR APPT PROVIDER
     C                   MOVE      *ZEROS        SCMOD3                         SCH DR APPT MODIFIER
     C                   MOVE      *BLANKS       SC4NAM                         SCH DR APPT PROV NAM
     C                   MOVE      *BLANKS       SC4AD1                         SCH DR APPT PROV ADR
     C                   MOVE      *BLANKS       SC4CTY                         SCH DR APPT PROV CIT
     C                   MOVE      *BLANKS       SC4ST                          SCH DR APPT PROV ST
     C                   MOVE      *ZEROS        SC4ZP1                         SCH DR APPT PROV ZIP
     C                   MOVE      *ZEROS        SC4ZP2                         SCH DR APPT PROV ZIP
     C                   MOVE      *ZEROS        SCAPDT                         APPOINTMENT DATE
     C                   MOVE      *ZEROS        SCAPTM                         APPOINTMENT TIME
     C                   MOVE      *BLANKS       SCAPAP                         TIME AM/PM
     C                   MOVE      *ZEROS        SCDARA                         DOCTOR'S AREA CODE
     C                   MOVE      *ZEROS        SCDPH1                         DOCTOR'S PHONE #
     C                   MOVE      *ZEROS        SCDPH2                         DOCTOR'S PHONE #
     C                   ELSE
     C                   MOVE      CL9N1         SCFEN3                         SCH DR APPT PROVIDER
     C                   MOVE      CL3N1         SCMOD3                         SCH DR APPT MODIFIER
     C                   MOVEL     CL40A1        SC4NAM                         SCH DR APPT PROV NAM
     C                   MOVEL     CL40A2        SC4AD1                         SCH DR APPT PROV ADR
     C                   MOVEL     CL20A1        SC4CTY                         SCH DR APPT PROV CIT
     C                   MOVEL     CL2A1         SC4ST                          SCH DR APPT PROV ST
     C                   MOVE      CL5N1         SC4ZP1                         SCH DR APPT PROV ZIP
     C                   MOVEL     CL5N2         SC4ZP2                         SCH DR APPT PROV ZIP
     C                   Z-ADD     CLDT1         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCAPDT
     C                   MOVE      CL1A1         SCAPAP
     C     CLDT2         DIV       100           SCAPTM
     C                   MOVE      XXDARA        SCDARA
     C                   MOVE      XXDPH1        SCDPH1
     C                   MOVE      XXDPH2        SCDPH2
     C                   END
     C*
     C*  GET DEPT/DOCUMENT RECORD
     C*
     C     DOKEY         CHAIN     PDO01                              60
     C*
     C*  GET DEPT. RECORD
     C*
     C     DEKEY         CHAIN     PDE01                              60
     C*
     C*  GET CLAIM RECORD
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C*
     C*  GET CLAIMANT DEMOGRAPHIC RECORD
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C*
     C*   ASSIGN SIGN HEADER FIELDS
     C*
     C                   MOVEL     DEDPNM        SCDPNM
     C                   MOVEL     DEDPCD        SCDPCD
     C                   MOVEL     QQDCID        SCDCID
     C                   MOVEL     DODESC        SCDESC
     C                   MOVE      UPFND         SCFND
     C                   MOVE      FNNAME        SCNAME
     C                   MOVE      CLFYR         SCFYR
     C                   MOVE      CMCASE        SCFCL2
      *
     C     CDLSN         CAT(P)    ',':0         SCCLNM
     C                   CAT       CDFSN:1       SCCLNM
     C     CDMID         IFNE      *BLANK
     C                   CAT       CDMID:1       SCCLNM
     C                   CAT       '.':0         SCCLNM
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C********************************
     C*   PROCESS ALL COMMAND KEYS   *
     C********************************
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*****************
     C*   F3 = EXIT   *
     C*****************
     C*
     C     *INKC         IFEQ      '1'
     C*
     C*          BEFORE RETURNING, DELETE THE RECORD IN WDCLL.
     C*
     C     CLKEY         DELETE    PCL01                              59
     C*
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C***********************
     C*   F10 = PAYEE SEARCH *
     C***********************
     C     *INKJ         IFEQ      '1'
     C     HFLD          IFEQ      'SCFEN3'
     C     HFLD          OREQ      'SCMOD3'
     C                   MOVE      SCFEN3        QQFEIN
     C                   MOVE      SCMOD3        QQMOD
     C                   MOVE      '2'           QQCALL
     C                   CALL      'W0030R'
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQRTCD        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    *IN60
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN33
     C                   MOVE      QQFEIN        XXFEIN
     C                   MOVE      QQMOD         XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     XXFEIN        SCFEN3
     C                   Z-ADD     XXMOD         SCMOD3
     C                   MOVEL     PDNAME        SC4NAM
      **** BEGIN ADD **** 04/12/99 ************************************
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL     PDADR2        SC4AD1
     C                   ELSE
      **** END   ADD **** 04/12/99 ************************************
     C                   MOVEL     PDADR1        SC4AD1
      **** BEGIN ADD **** 04/12/99 ************************************
     C                   ENDIF
      **** END   ADD **** 04/12/99 ************************************
     C                   MOVEL     PDCTY         SC4CTY
     C                   MOVEL     PDST          SC4ST
     C                   MOVEL     PDZIP         SC4ZP1
     C                   MOVE      PDZIP         SC4ZP2
     C                   Z-ADD     PDAREA        SCDARA
     C                   MOVEL     PDPHN#        SCDPH1
     C                   MOVE      PDPHN#        SCDPH2
     C                   MOVE      'Y'           XXSKP3
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    *IN60
     C                   END                                                    QQRTCD = '*'
     C*
     C                   END                                                    HFLD = SCFEN3
     C                   END                                                    END SCR11
     C*                                                    OR  SCR14
     C********************
     C*   F12 = RETURN   *
     C********************
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*          BEFORE RETURNING, DELETE THE RECORD IN WDCLL.
     C*
     C     CLKEY         DELETE    PCL01                              59
     C*
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ELSE
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*****************************
     C*   PROCESS HELP REQUESTS   *
     C*****************************
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C******************************
     C*   PROCESS REQUEST SCREEN   *
     C******************************
     C*
     C     SCR01T        TAG
     C*
     C*  EITHER FEDERAL ID# OR NAME MUST BE ENTERED
     C*
     C     XXSKP3        CABEQ     'Y'           SKIP3
     C     SCFEN3        IFEQ      *ZERO
     C     SCMOD3        ANDEQ     *ZERO
     C     SC4NAM        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF ENTERED, FED ID# MUST EXIST
     C*
     C     SCFEN3        IFNE      *ZERO
     C     SCMOD3        ORNE      *ZERO
     C                   Z-ADD     SCFEN3        XXFEIN
     C                   Z-ADD     SCMOD3        XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ELSE
     C     SC4NAM        IFEQ      *BLANKS
     C                   MOVEL     PDNAME        SC4NAM
      **** BEGIN ADD **** 04/12/99 ************************************
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL     PDADR2        SC4AD1
     C                   ELSE
      **** END   ADD **** 04/12/99 ************************************
     C                   MOVEL     PDADR1        SC4AD1
      **** BEGIN ADD **** 04/12/99 ************************************
     C                   ENDIF
      **** END   ADD **** 04/12/99 ************************************
     C                   MOVEL     PDCTY         SC4CTY
     C                   MOVEL     PDST          SC4ST
     C                   MOVEL     PDZIP         SC4ZP1
     C                   MOVE      PDZIP         SC4ZP2
     C                   END
     C     SCDARA        IFEQ      *ZEROS
     C     SCDPH1        ANDEQ     *ZEROS
     C     SCDPH2        ANDEQ     *ZEROS
     C                   Z-ADD     PDAREA        SCDARA
     C                   MOVEL     PDPHN#        SCDPH1
     C                   MOVE      PDPHN#        SCDPH2
     C                   END
     C                   MOVE      'Y'           XXSKP3            1
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  VALIDATE ADDRESS
     C*
     C     SC4NAM        IFNE      *BLANKS
     C*
     C     SC4AD1        COMP      *BLANKS                                23      ADDRESS
     C     *IN23         CABEQ     '1'           STRT
     C     SC4CTY        COMP      *BLANKS                                24      CITY
     C     *IN24         CABEQ     '1'           STRT
     C*******    SC4ST     LOKUPST3                      60  STATE
     C                   MOVEL(P)  SC4ST         XXKEY
     C     STKY          CHAIN     PST01                              60          STATE
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C     SC4ZP1        IFLE      *ZERO                                          ZIP
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C     SKIP3         TAG
     C*
     C*  APPOINTMENT DATE MUST BE ENTERED AND VALID
     C*
     C                   Z-ADD     SCAPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCAPDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*  APPOINTMENT TIME MUST BE ENTERED AND VALID
     C*
     C*  SINCE TIME WILL NOW BE NON-MILITARY VALID VALUES
     C*  CAN ONLY BE FROM 1:00 AM/PM TO 12:59 AM/PM
     C*
     C     SCAPTM        IFLT      100
     C     SCAPTM        ORGT      1259
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C*  MINUTES AFTER THE HOUR MUST BE BETWEEN 0 AND 59
     C*
     C                   MOVE      SCAPTM        XXWK20            2 0
     C     XXWK20        IFGT      59
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C*  MUST SPECIFY A FOR AM OR P FOR PM
     C*
     C     SCAPAP        IFNE      'A'
     C     SCAPAP        ANDNE     'P'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C*  DOCTOR'S PHONE NUMBER MUST BE ENTERED
     C*
     C     SCDARA        IFEQ      *ZEROS
     C     SCDPH1        OREQ      *ZEROS
     C     SCDPH2        OREQ      *ZEROS
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*  WE HAVE PASSED ALL EDITS, PROCESS SCREEN DATA
     C*
     C     CLKEY         CHAIN     PCL01                              50
     C                   MOVE      SCFEN3        CL9N1                          SCH DR APPT PROVIDER
     C                   MOVE      SCMOD3        CL3N1                          SCH DR APPT MODIFIER
     C                   MOVEL     SC4NAM        CL40A1                         SCH DR APPT PROV NAM
     C                   MOVEL     SC4AD1        CL40A2                         SCH DR APPT PROV ADR
     C                   MOVEL     SC4CTY        CL20A1                         SCH DR APPT PROV CIT
     C                   MOVEL     SC4ST         CL2A1                          SCH DR APPT PROV ST
     C                   Z-ADD     SC4ZP1        CL5N1                          SCH DR APPT PROV ZIP
     C                   Z-ADD     SC4ZP2        CL5N2                          SCH DR APPT PROV ZIP
     C                   Z-ADD     SCAPDT        CLDT1                          APPOINTMENT DATE
     C                   MOVE      SCAPAP        CL1A1                          A.M./P.M.
     C     SCAPTM        MULT      100           CLDT2                          APPT TIME
     C                   MOVE      SCDARA        XXDARA                         DOCTOR'S AREA CODE
     C                   MOVE      SCDPH1        XXDPH1                         DOCTOR'S PHONE #
     C                   MOVE      SCDPH2        XXDPH2                         DOCTOR'S PHONE #
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      CL2A1         XXST              2
     C                   Z-ADD     XXCZIP        XXZIP
     C                   MOVEL     CL20A1        XXCTY            25
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         CL40A3
     C*
     C     *IN50         IFEQ      '0'
     C                   WRITE     PC201
     C                   END
     C* ---------------------------------------------------------------
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C****************************************************
     C*   SUBROUTINE TO LOAD THE CITY STATE & ZIP        *
     C****************************************************
     C     SRADDR        BEGSR
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXST          XXST13           13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ            40
     C                   ENDSR
     *****  TABLE ST
     ***ALAKAZARCACOCTDEDCFLGAHIIDILINIAKSKYLAMEMDMAMIMNMSWY
     ***MOMTNENVNHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIPR
