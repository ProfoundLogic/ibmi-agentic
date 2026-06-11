     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W4110R09 - C-22  NOTICE OF FIRST PAYMENT OF COMPENSATION
     H*****************************************************************
     H* PROGRAM DESCRIPTION                                           *
     H*                                                               *
     H* This program prompts the user for additional input necessary  *
     H* to print a department letter.                                 *
     H*                                                               *
     H*  CALLED FROM:  W4110R                                         *
     H*                                                               *
     H*  CALLS TO:     NONE                                           *
     H*                                                               *
     H*  PROGRAMMER:   C:02                                           *
     H*                                                               *
     H*  CREATED:    09/24/98               PROJECT: WC859            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/04/99 C:15 Project: WC577                                  *
     H*               Removed the reference to file WMEIP since this  *
     H*               file no longer exists.                          *
     H*****************************************************************
     FW4110F09  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      *  COMPENSATION REIMBURSEMENT FILE
     FWDRBL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDC2L     UF A E           K DISK
     FWDCLL     UF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMDEL     IF   E           K DISK
     FWMDOL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWDPAL5    IF   E           K DISK
     FWMEML     IF   E           K DISK
     F*** BEGIN DLT *** 03/04/99 **************************************
     F******WMEIL1  IF  E           K        DISK
     F*** END DLT ***** 03/04/99 **************************************
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D                 DS
     D  XXDATE                 1     10
     D  XXMM                   1      2
     D  XXSLH1                 3      3
     D  XXDD                   4      5
     D  XXSLH2                 6      6
     D  XXCCYY                 7     10
     D                 DS
     D  X1DATE                 1      8
     D  X1MM                   1      2
     D  X1DD                   3      4
     D  X1CCYY                 5      8
      *
     D                 DS
     D  CMSS#                  1      9
     D  X1SS1                  1      3
     D  X1SS2                  4      5
     D  X1SS3                  6      9
      *
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
     C*  EMPLOYER MASTER KEY LIST
     C*
     C     EMKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
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
     C*  FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CLFYR
      *
      *   KEY LIST FOR COMP REIMB FILE
      *
     C     RBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXJRST
     C                   KFLD                    XXEFDT
      *
     C     RBKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXJRST
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*  CLAIMS CHECK PAYMENT FILE
     C*
     C     PAKEY         KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLFYR
     C                   KFLD                    CLFCLM
     C                   KFLD                    XXTRDT            7 0
     C*
     C     PAKEY1        KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLFYR
     C                   KFLD                    CLFCLM
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    CLFND         XXFND
     C     *LIKE         DEFINE    CLREQ#        XXREQ#
     C     *LIKE         DEFINE    DEDEPT        XXDEPT
     C     *LIKE         DEFINE    CMACDT        XXEFDT
     C     *LIKE         DEFINE    CMJRST        XXJRST
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
     C     *IN60         IFEQ      *ON
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
     C     *IN50         IFEQ      *ON
      * SCREEN 1
     C                   Z-ADD     *ZERO         SCDTFP
     C                   MOVE      *BLANKS       SCBN01
     C                   MOVE      *BLANKS       SCBN02
     C                   MOVE      *BLANKS       SCBN03
     C                   MOVE      *BLANKS       SCBN04
     C                   MOVE      *BLANKS       SCBN05
      *
     C                   ELSE
      *  SCREEN 1
     C     CLKEY         CHAIN     PC201                              60
     C                   MOVE      CL10A1        SCDTFP
     C                   MOVEL     CL2A1         SCBN01
     C                   MOVEL     CL2A2         SCBN02
     C                   MOVEL     CL2A3         SCBN03
     C                   MOVEL     CL2A4         SCBN04
     C                   MOVEL     CL2A5         SCBN05
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
     C*   LOAD HEADER FIELDS
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
     C                   MOVE      *BLANKS       SCCLNM                         FIRST M. LAST
     C     CDFSN         CAT       CDMID:1       SCCLNM
     C     SCCLNM        CAT       '.':0         SCCLNM
     C     SCCLNM        CAT       CDLSN:1       SCCLNM
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
      * ===============================================================
      *              M A I N L I N E
      * ===============================================================
      *
     C     *IN03         DOUEQ     *ON
      *
      *   DISPLAY THE APPROPRIATE SCREEN
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
      *
      * RESET INDICATORS 30 - 35
      *
     C                   MOVEA     '000000'      *IN(30)
     C*
     C********************************
     C*   PROCESS ALL COMMAND KEYS   *
     C********************************
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*****************
     C*   F3 = EXIT   *
     C*****************
     C*
     C     *INKC         IFEQ      *ON
     C*
     C*          BEFORE RETURNING, DELETE THE RECORD IN WDCLL.
     C*
     C     CLKEY         DELETE    PCL01                              59
     C*
     C                   MOVE      'A'           QQRTCD
     C                   LEAVE
     C                   END
     C*
     C********************
     C*   F12 = RETURN   *
     C********************
     C*
     C     *INKL         IFEQ      *ON
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*          BEFORE RETURNING, DELETE THE RECORD IN WDCLL.
     C*
     C     CLKEY         DELETE    PCL01                              59
     C*
     C                   MOVE      'B'           QQRTCD
     C                   LEAVE
     C                   END
     C                   END
     C*
     C*****************************
     C*   PROCESS HELP REQUESTS   *
     C*****************************
     C*
     C     *IN10         IFEQ      *ON
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
     C                   ITER
     C                   END
      *
     C                   END                                                    *IN11
      *
      *   DISPLAY THE APPROPRIATE SCREEN
      *
     C     Q2RECF        CASEQ     'SCR01'       SCR01T
     C                   ENDCS
      *
     C                   ENDDO                                                  *IN03
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C                   MOVE      *ON           *INLR
     C                   RETURN
      *
      ******************************************************************
      *   PROCESS REQUEST SCREEN 1
      ******************************************************************
     C     SCR01T        BEGSR
      *
      * VALIDATE RESPONSES
      *
     C     *IN30         DOUEQ     *OFF
      *
     C     SCDTFP        IFNE      *ZERO
     C                   Z-ADD     SCDTFP        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTFP
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN30                          GEN ERROR IND
     C                   MOVE      *ON           *IN31
     C                   LEAVE
     C                   END
     C                   END
      *
      *  WE HAVE PASSED ALL EDITS, PROCESS SCREEN DATA
      *
     C     *IN30         IFEQ      *OFF
      *
     C                   MOVEL     SCDTFP        X1DATE
     C                   MOVE      X1MM          XXMM
     C                   MOVE      X1DD          XXDD
     C                   MOVE      X1CCYY        XXCCYY
     C                   MOVE      '/'           XXSLH1
     C                   MOVE      '/'           XXSLH2
     C                   MOVE      XXDATE        CL10A1
      *
     C     SCBN01        IFEQ      '1'
     C                   MOVEL     'XX'          CL2A1
     C                   END
      *
     C     SCBN02        IFEQ      '1'
     C                   MOVEL     'XX'          CL2A2
     C                   END
      *
     C     SCBN03        IFEQ      '1'
     C                   MOVEL     'XX'          CL2A3
     C                   END
      *
     C     SCBN04        IFEQ      '1'
     C                   MOVEL     'XX'          CL2A4
     C                   END
      *
     C     SCBN05        IFEQ      '1'
     C                   MOVEL     'XX'          CL2A5
     C                   END
      *
     C                   END                                                    *IN30
      *
     C     *IN30         IFEQ      *ON
     C                   MOVEL     'SCR01'       Q2RECF
     C                   ELSE
     C                   MOVE      *ON           *IN03
     C                   EXSR      SRUPD
     C                   END
      *
     C                   ENDDO
      *
     C                   ENDSR
      *
      * ===============================================================
      *   UPDATE THE FILE
      * ===============================================================
     C     SRUPD         BEGSR
      *
     C     X1SS1         CAT       '-':0         CL20A2                         CLAIMANT SS#
     C     CL20A2        CAT       X1SS2:0       CL20A2
     C     CL20A2        CAT       '-':0         CL20A2
     C     CL20A2        CAT       X1SS3:0       CL20A2
      *
     C                   MOVE      CMCO#         X1CO#             3            FUN YEAR
     C                   MOVE      CMFND         X1FND             3            CASE #
     C                   MOVE      CMFYR         X1FYR             3            FUN YEAR
     C                   MOVE      CMCASE        X1CASE            7            CASE #
     C     X1CO#         CAT       '-':0         CL20A4
     C     CL20A4        CAT       X1FND:0       CL20A4
     C     CL20A4        CAT       '-':0         CL20A4
     C     CL20A4        CAT       X1FYR:0       CL20A4
     C     CL20A4        CAT       '-':0         CL20A4
     C     CL20A4        CAT       X1CASE:0      CL20A4
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C                   MOVEL     SRDTO         X1DATE
     C                   MOVE      X1MM          XXMM
     C                   MOVE      X1DD          XXDD
     C                   MOVE      X1CCYY        XXCCYY
     C                   MOVE      '/'           XXSLH1
     C                   MOVE      '/'           XXSLH2
     C                   MOVE      XXDATE        CL10A2
     C                   Z-ADD     CMDBDT        SRDTI
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C                   MOVEL     SRDTO         X1DATE
     C                   MOVE      X1MM          XXMM
     C                   MOVE      X1DD          XXDD
     C                   MOVE      X1CCYY        XXCCYY
     C                   MOVE      '/'           XXSLH1
     C                   MOVE      '/'           XXSLH2
     C                   MOVE      XXDATE        CL10A3
      *
      *  SHORTEN FEILDS TO 30A
      *
     C                   MOVEL     CDFSN         XX10A            10
     C                   MOVEL     CDLSN         XX19A            19
     C     XX10A         CAT       XX19A:1       CL40A4                         CLAIMANT NAME
      *
     C                   MOVE      *BLANKS       XX30A            30
     C                   MOVEL     CDADR1        XX30A
     C                   MOVEL     XX30A         CL40A5                         ADDRESS
      *
     C                   MOVE      *BLANKS       XX30A
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL(P)  FNANAM        XX30A
     C                   MOVEL(P)  FNANAM        CLCNME
     C                   MOVEL(P)  FNAAD1        CLCAD1
     C                   MOVE      *BLANKS       CLCAD2
     C                   MOVEL(P)  FNACTY        CLCCSZ
     C     CLCCSZ        CAT       ',':0         CLCCSZ
     C     CLCCSZ        CAT       FNAST:1       CLCCSZ
     C                   MOVEL     FNAZIP        XXZIP1            5
     C                   MOVE      FNAZIP        XXZIP2            4
     C     CLCCSZ        CAT       XXZIP1:1      CLCCSZ
     C     XXZIP2        IFNE      '0000'
     C     CLCCSZ        CAT       '-':0         CLCCSZ
     C     CLCCSZ        CAT       XXZIP2:0      CLCCSZ
     C                   ENDIF
     C                   ELSE
     C                   MOVEL(P)  CONAME        XX30A            30
     C                   ENDIF
     C                   MOVEL     XX30A         CL40A6                         INSURER/CO NAME
      *
      *  GET EMPLOYER INFO FROM EMPLOYER MASTER
      *
     C     EMKEY         CHAIN     PEM01                              62
     C     *IN62         IFEQ      *OFF
     C                   MOVE      *BLANKS       XX30A
     C     EMNAME        IFNE      *BLANKS
     C                   MOVEL     EMNAME        XX30A
     C                   ELSE
     C                   MOVEL     EMDBA         XX30A
     C                   END
     C                   MOVEL     XX30A         CL40A7                         EMPLOYER
      *
     C                   MOVE      *BLANKS       XX30A
     C                   MOVEL     EMMAD1        XX30A
     C                   MOVEL     XX30A         CL40A8                         EMP ADDRESS
     C                   END
     C                   Z-ADD     CMAWW         CL9N2                          COMP RATE
     C*** BEGIN CHG *** 02/18/99 **************************************
     C**********           Z-ADDCMCRT     CL9N2            COMP RATE
     C*********************Z-ADDCMCRT     CL9N3            COMP RATE
     C*** END CHG *** 02/18/99 ****************************************
      *
      *  GET FUND DETAIL FILE TO CALC RATE OF WEEKLY COMPENSATION
      *  EX.:  F1REPC = 66.67
      *
     C     F1KEY         CHAIN     PF101                              60
     C*** BEGIN ADD *** 03/04/99 **************************************
     C                   MOVEL     CMJRST        XXJRST
     C                   Z-ADD     CMACDT        XXEFDT
     C     RBKY          SETGT     PRB01
     C     RBKY1         READPE    PRB01                                  60
     C     *IN60         IFEQ      *OFF
      *** BEG CHG *** 06/02/04 ****************************************
      *** BEG CHG *** 06/02/04 ****************************************
     C********** RBREPC    DIV  100       XXREPC  54H
     C********** CMAWW     MULT XXREPC    CL9N3     H        CALC COMP RATE
     C     RBCMPR        DIV(H)    100           XXCMPR            7 4
     C     CMAWW         MULT(H)   XXCMPR        CL9N3                            CALC COMP RATE
      *** END CHG *** 06/02/04 ****************************************
      *
      *  COMP RATE CAN NOT BE LESS THAN THE MINIMUM
      *
     C     CL9N3         IFLT      RBMNRT
     C                   Z-ADD     RBMNRT        CL9N3
     C                   ENDIF                                                  SCCCRT<RBMNRT
      *
      *  COMP RATE CAN NOT BE GREATER THAN THE MAXIMUM
      *
     C     CL9N3         IFGT      RBPACP
     C                   Z-ADD     RBPACP        CL9N3
     C                   ENDIF                                                  SCCCRT<RBPACP
     C*** END ADD *** 03/04/99 ****************************************
     C                   END
      *
      *   EDI CLAIM STATE INFO
      *
     C*** BEGIN DLT *** 03/04/99 **************************************
     C********** CMKEY     CHAINPEI01                60
     C********** *IN60     IFEQ *OFF
     C**********           MOVELEIACL#    CL20A1           STATE FILE #
     C**********           END
     C*** END DLT ***** 03/04/99 **************************************
     C*** BEGIN ADD *** 03/04/99 **************************************
     C                   MOVEL     CMACL#        CL20A1                         STATE FILE #
     C*** END ADD ***** 03/04/99 **************************************
      *
      *  GET THE INFO FOR THE FIRST COMPENSATION RECORD ONLY
      *
     C                   Z-ADD     *ZERO         XXTRDT                         TRANS DATE
     C     PAKEY         SETLL     PPA01
      *
     C     *IN61         DOUEQ     *ON
     C     PAKEY1        READE     PPA01                                  61
     C     *IN61         IFEQ      *OFF
      *
     C     PARETP        IFEQ      'C'                                          COMP ONLY
     C                   Z-ADD     PAFRDT        SRDTI
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C                   MOVEL     SRDTO         X1DATE
     C                   MOVE      X1MM          XXMM
     C                   MOVE      X1DD          XXDD
     C                   MOVE      X1CCYY        XXCCYY
     C                   MOVE      '/'           XXSLH1
     C                   MOVE      '/'           XXSLH2
     C                   MOVE      XXDATE        CL10A4
     C                   Z-ADD     PATODT        SRDTI
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C                   MOVEL     SRDTO         X1DATE
     C                   MOVE      X1MM          XXMM
     C                   MOVE      X1DD          XXDD
     C                   MOVE      X1CCYY        XXCCYY
     C                   MOVE      '/'           XXSLH1
     C                   MOVE      '/'           XXSLH2
     C                   MOVE      XXDATE        CL10A5
     C                   Z-ADD     PAAMT         CL9N1                          PAYMENT AMOUNT
     C                   LEAVE
     C                   END                                                    PARETP = C
      *
     C                   END                                                    *IN61
     C                   ENDDO                                                  *IN61
      *
     C     *IN50         IFEQ      *OFF
     C                   UPDATE    PCL01
     C                   WRITE     PC201
     C                   END
      *
     C                   ENDSR
      *
