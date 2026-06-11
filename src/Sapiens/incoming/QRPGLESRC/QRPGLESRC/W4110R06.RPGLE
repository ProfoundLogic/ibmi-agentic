     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W4110R06 - C-29  FINAL REPORT OF PAYMENT
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
     H* 06/02/04 C:53 PROJECT: WC1180                                 *
     H*               EXPAND FROM 2 DEC POS TO 4                      *
     H*                                                               *
     H* 07/17/03 C:53 Support: WC065/38013                            *
     H*               Correct weeks/days calc.  When a neg recovery   *
     H*               is processed, the weeks/days should be added.   *
     H*                                                               *
     H*  08/29/02  C:132 SEQ#: WC967
     H*                  Add code for settlement amounts tha include  *
     H*                  Attorney's Fees. This new pay value is a "U" *
     H*                  and would be similar to the pay value "L"    *
     H*                  This new pay walue is for 1099 Box 14 Amts.  *
     H*                                                               *
     H* 11/10/99 C:15 Project: WC577                                  *
     H*               Corrected the loading of the comp pmt days.     *
     H*                                                               *
     H* 03/04/99 C:15 Project: WC577                                  *
     H*               Removed the reference to file WMEIP since this  *
     H*               file no longer exists.                          *
     H*****************************************************************
     FW4110F06  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      *  COMPENSATION REIMBURSEMENT FILE
     FWDRBL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDCLL     UF   E           K DISK
     FWDC2L     UF A E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMDEL     IF   E           K DISK
     FWMDOL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F*** BEGIN DLT *** 03/04/99 **************************************
     F******WMEIL1  IF  E           K        DISK
     F*** END DLT ***** 03/04/99 **************************************
     FWDPAL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PRT01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PBS01)
     F                                     IGNORE(PTT01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PVS01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PSY01)
      *

     D WK              S              3  0 DIM(5)                               WEEKS
     D DY              S              3  0 DIM(5)                               DAYS
      *
     D DATEDS          DS
     D  MDY                    3      8  0
     D  MM                     3      4
     D  DD                     5      6
     D  MD                     3      6  0
     D  YY2                    7      8  0
     D  YY1                    1      2  0
     D  YMD                    1      6  0
     D*
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
     D OPTNM           C                   CONST('Additional Input -
     D                                     Screen')
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

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
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    QQDPCD
     C*
     C*  DEPT/DOC ID MASTER FILE
     C*
     C     DOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    QQDPCD
     C                   KFLD                    QQFLDR
     C                   KFLD                    QQDCID
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    UPFND
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
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    CLFND         XXFND
     C     *LIKE         DEFINE    CLREQ#        XXREQ#
     C     *LIKE         DEFINE    DEDEPT        XXDEPT
     C     *LIKE         DEFINE    B2KEY         XXKEY
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
     C                   MOVE      *BLANKS       SCSEMP
     C                   MOVE      *BLANKS       SCDEMP
     C                   MOVE      *BLANKS       SCWAGC
     C                   Z-ADD     *ZERO         SCWBF
     C                   Z-ADD     *ZERO         SCWAF
     C                   MOVE      *BLANKS       SCRNS1
     C                   MOVE      *BLANKS       SCRNS2
     C                   MOVE      *BLANKS       SCCNP1
     C                   MOVE      *BLANKS       SCCNP2
      *
     C                   ELSE
      * SCREEN 1
     C     CLKEY         CHAIN     PC201                              60
     C                   MOVEL     CL2A1         SCSEMP
     C                   MOVEL     CL2A2         SCDEMP
     C                   MOVEL     CL2A3         SCWAGC
     C                   Z-ADD     CL9N1         SCWBF
     C                   Z-ADD     CL9N2         SCWAF
     C                   MOVEL     CL60A1        SCRNS1
     C                   MOVEL     CL60A2        SCRNS2
     C                   MOVEL     CL60A3        SCCNP1
     C                   MOVEL     CL60A4        SCCNP2
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
      * RESET INDICATORS 30 - 45
      *
     C                   MOVEA     '00000000'    *IN(30)
     C                   MOVEA     '00000000'    *IN(38)
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
     C*
     C*          BEFORE RETURNING, DELETE THE RECORD IN WDCLL.
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     CLKEY         DELETE    PCL01                              59
     C                   MOVE      'B'           QQRTCD
     C                   LEAVE
     C                   END
     C*
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
      *   EDIT THE APPROPRIATE SCREEN
      *
     C     Q2RECF        CASEQ     'SCR01'       SCR01T
     C                   ENDCS
      *
     C                   ENDDO                                                  *IN03
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C                   MOVE      *ON           *INLR
     C                   RETURN
      *
      * ****************************************************************
      *   PROCESS REQUEST SCREEN 1
      * ****************************************************************
     C     SCR01T        BEGSR
      *
     C     *IN30         DOUEQ     *OFF
      *
      * VALIDATE RESPONSES
      *
      *  WE HAVE PASSED ALL EDITS, PROCESS SCREEN DATA
      *
     C     *IN30         IFEQ      *OFF
     C                   MOVEL     SCSEMP        CL2A1
     C                   MOVEL     SCDEMP        CL2A2
     C                   MOVEL     SCWAGC        CL2A3
     C                   Z-ADD     SCWBF         CL9N1
     C                   Z-ADD     SCWAF         CL9N2
     C                   MOVEL     SCRNS1        CL60A1
     C                   MOVEL     SCRNS2        CL60A2
     C                   MOVEL     SCCNP1        CL60A3
     C                   MOVEL     SCCNP2        CL60A4
     C                   END
      *
     C     *IN30         IFEQ      *ON
     C                   MOVEL     'SCR01'       Q2RECF
     C                   ELSE
     C                   MOVE      *ON           *IN03
     C                   EXSR      SRCALC
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
     C                   MOVEL     CDFSN         XXFSN             7            FIRST NAME
     C                   MOVEL     CDLSN         XXLSN            16            LAST
     C     XXFSN         CAT       CDMID:1       XXCLNM           27
     C     CDMID         IFNE      *BLANK                                       DON'T PRT '.'
     C     XXCLNM        CAT       '.':0         XXCLNM                          IF NO MID INTIAL
     C                   END
     C     XXCLNM        CAT       XXLSN:1       CL40A5
     C*** BEGIN ADD *** 02/19/99 **************************************
     C                   MOVEL(P)  CDADR1        CL40AA
     C*** END ADD *** 02/19/99 ****************************************
      *
     C                   MOVE      CLCO#         X1CO#             3
     C                   MOVE      CLFND         X1FND             3
     C                   MOVE      CLFYR         X1FYR             3
     C                   MOVE      CLFCLM        X1CASE            7
      *
     C     X1CO#         CAT       '-':0         CL20A3                         CO,FUND,FYR,CASE
     C     CL20A3        CAT       X1FND:0       CL20A3
     C     CL20A3        CAT       '-':0         CL20A3
     C     CL20A3        CAT       X1FYR:0       CL20A3
     C     CL20A3        CAT       '-':0         CL20A3
     C     CL20A3        CAT       X1CASE:0      CL20A3                         INSURER FILE #
      *
      *  SPLIT YMD DATES TO MM/DD  &  YY
      *
     C                   CLEAR                   DATEDS
     C                   MOVE      CMACDT        YMD                            ACCIDENT DATE
     C                   Z-ADD     YY1           YY2
     C     MM            CAT       '/':0         CL20A5
     C     CL20A5        CAT       DD:0          CL20A5                         MM/DD
     C                   MOVE      YY2           CL2A5                          19YY
      *
     C                   CLEAR                   DATEDS
     C                   MOVE      CMDTLW        YMD                            LAST WORKED DT
     C                   Z-ADD     YY1           YY2
     C     MM            CAT       '/':0         CL20A6
     C     CL20A6        CAT       DD:0          CL20A6                         MM/DD
     C                   MOVE      YY2           CL2A6                          19YY
      *
     C                   CLEAR                   DATEDS
     C*** BEGIN ADD *** 02/19/99 **************************************
     C     CMRLW         IFGT      *ZEROS
     C                   MOVE      CMRLW         YMD                            LAST WORKED DT
     C                   Z-ADD     YY1           YY2
     C     MM            CAT       '/':0         CL20A7
     C     CL20A7        CAT       DD:0          CL20A7                         MM/DD
     C                   ELSE
     C                   MOVE      *BLANKS       CL20A7
     C                   ENDIF
     C*** BEGIN ADD *** 02/19/99 **************************************
     C     YY2           IFGT      *ZEROS
     C                   MOVE      YY2           CL2A7                          19YY
     C                   ELSE
     C                   MOVE      *BLANKS       CL2A7
     C                   ENDIF
     C*** END ADD *** 02/19/99 ****************************************
      *
     C                   CLEAR                   DATEDS
     C*** BEGIN ADD *** 02/19/99 **************************************
     C     CMRTW         IFGT      *ZEROS
     C                   MOVE      CMRTW         YMD                            LAST WORKED DT
     C                   Z-ADD     YY1           YY2
     C     MM            CAT       '/':0         CL20A8
     C     CL20A8        CAT       DD:0          CL20A8                         MM/DD
     C                   ELSE
     C                   MOVE      *BLANKS       CL20A8
     C                   ENDIF
     C*** END ADD *** 02/19/99 ****************************************
     C*** BEGIN ADD *** 02/19/99 **************************************
     C     YY2           IFGT      *ZEROS
     C                   MOVE      YY2           CL2A8                          19YY
     C                   ELSE
     C                   MOVE      *BLANKS       CL2A8
     C                   ENDIF
     C*** END ADD *** 02/19/99 ****************************************
      *
     C                   MOVE      CMCO#         X1CO#             3            COMPANY #
     C                   MOVE      CMFND         X1FND             3            FUND #
     C                   MOVE      CMFYR         X1FYR             3            FUN YEAR
     C                   MOVE      CMCASE        X1CASE            7            CASE #
     C     X1CO#         CAT       '-':0         CL20A4
     C     CL20A4        CAT       X1FND:0       CL20A4
     C     CL20A4        CAT       '-':0         CL20A4
     C     CL20A4        CAT       X1FYR:0       CL20A4
     C     CL20A4        CAT       '-':0         CL20A4
     C     CL20A4        CAT       X1CASE:0      CL20A4                         CLAIM #
      *
      *  CLAIMANT'S AGE AT TIME OF ACCIDENT
      *
     C     CMACDT        SUB       CDDTOB        X1DAYS            6 0
     C                   MOVEL     X1DAYS        CL2A9
      *
      *  GET FUND DETAIL FILE TO CALC RATE OF WEEKLY COMPENSATION
      *  EX.:  F1REPC = 66.67
      *
     C**********           Z-ADDCMCRT     CL9N3            COMP RATE
     C*** BEGIN ADD *** 03/04/99 **************************************
     C                   MOVEL     CMJRST        XXJRST
     C                   Z-ADD     CMACDT        XXEFDT
     C     RBKY          SETGT     PRB01
     C     RBKY1         READPE    PRB01                                  60
     C     *IN60         IFEQ      *OFF
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
     C                   END
     C*** END ADD *** 03/04/99 ****************************************
     C                   Z-ADD     CMAWW         CL9N4
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      *OFF
     C                   END
      *
      *   EDI CLAIM INFORMATION
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
     C     *IN50         IFEQ      '0'
     C                   UPDATE    PCL01
     C                   WRITE     PC201
     C                   END
      *
     C                   ENDSR
      *
      * ===============================================================
      *   CALCULATE COMPENSATION PAYMENT TOTALS
      * ===============================================================
     C     SRCALC        BEGSR
     C*
     C*  START READING PAYMENT FILE
     C*
     C     CMKEY         CHAIN     PPA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*   CALCULATE WEEKS & DAYS
     C*
     C     PATYPE        IFEQ      'P'
     C     PAAMT         ANDGE     0
     C     PATYPE        OREQ      'Z'
     C     PAAMT         ANDGE     0
     C     PATYPE        OREQ      'A'
     C     PAAMT         ANDGE     0
     C     PATYPE        OREQ      'M'
     C     PAAMT         ANDGE     0
     C*** BEG ADD *** 07/17/03 **************************************
     C     PATYPE        OREQ      'R'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'S'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'V'
     C     PAAMT         ANDLT     0
     C*** END ADD *** 07/17/03 **************************************
     C                   ELSE
     C                   MULT      -1            PAWKS
     C                   MULT      -1            PADAYS
     C                   END
     C*
     C*  GET THE PMT VAL 1 PLACEMENT VALUE TO DETERMINE WHICH
     C*  CATEGORY THE AMOUNT WILL BE ADDED TO.
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     PABR2         XXKEY
     C     TBKY          CHAIN     PB201                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        B2PLCD
     C                   END
     C*
     C     PATYPE        IFNE      'P'
     C     PATYPE        ANDNE     'M'
     C     PATYPE        ANDNE     'A'
     C     PATYPE        ANDNE     'Z'
     C                   MULT      -1            PAAMT
     C                   END
     C*
     C     B2PLCD        IFEQ      '1'                                          TEMP TOTAL
     C     PARETP        ANDEQ     'C'
     C                   ADD       PAAMT         CL11N1
     C                   ADD       PAWKS         WK(1)
     C                   ADD       PADAYS        DY(1)
     C                   END
     C*
     C     B2PLCD        IFEQ      '7'                                          TEMP PARTIAL
     C     PARETP        ANDEQ     'C'
     C                   ADD       PAAMT         CL11N2
     C                   ADD       PAWKS         WK(2)
     C                   ADD       PADAYS        DY(2)
     C                   END
     C*
     C     B2PLCD        IFEQ      '9'                                          PERM PARTIAL
     C     PARETP        ANDEQ     'C'
     C                   ADD       PAAMT         CL11N3
     C                   ADD       PAWKS         WK(3)
     C                   ADD       PADAYS        DY(3)
     C                   END
     C*
     C     B2PLCD        IFEQ      '4'                                          PERM TOTAL
     C     PARETP        ANDEQ     'C'
     C                   ADD       PAAMT         CL11N4
     C                   ADD       PAWKS         WK(4)
     C                   ADD       PADAYS        DY(4)
     C                   END
     C*
     C     B2PLCD        IFEQ      'L'                                          LUMP SUM
     C     PARETP        ANDEQ     'C'
      **** BEGIN ADD **** 08/29/02 ***************************************
     C     B2PLCD        OREQ      'U'                                          LUMP SUM
     C     PARETP        ANDEQ     'C'
      ****** END ADD **** 08/29/02 ***************************************
     C                   ADD       PAAMT         CL11N5
     C                   END
     C*
     C     B2PLCD        IFEQ      '6'                                          DEATH CLAIM
     C     PARETP        ANDEQ     'C'
     C                   ADD       PAAMT         CL11N6
     C                   ADD       PAWKS         WK(5)
     C                   ADD       PADAYS        DY(5)
     C                   END
     C*
     C     PARETP        IFEQ      'C'                                          COMP
     C                   ADD       PAAMT         CL11N7
     C                   END
     C*
     C     PARETP        IFEQ      'M'                                          MED
     C                   ADD       PAAMT         CL11N8
     C                   END
     C*
     C     B2PLCD        IFEQ      'G'                                          FUNERAL EXP
     C     PARETP        ANDEQ     'C'
     C                   ADD       PAAMT         CL11N9
     C                   END
     C*
     C     PARETP        IFEQ      'E'                                          TOTAL LEGAL
     C                   ADD       PAAMT         CL11N0
     C                   END
     C*
     C     CMKEY         READE     PPA01                                  60
     C                   END
     C*
     C*  CONVERT THE DAYS INTO WEEKS
     C*
     C                   DO        5             X                 3 0
     C*
     C     DY(X)         IFGE      7
     C     DY(X)         ORLT      0
     C     DY(X)         DIV       7             XXWEEK            3 0
     C                   ADD       XXWEEK        WK(X)
     C                   MULT      7             XXWEEK
     C                   SUB       XXWEEK        DY(X)
     C                   END
     C*
     C     DY(X)         IFLT      0
     C                   SUB       1             WK(X)
     C                   ADD       7             DY(X)
     C                   END
     C*
     C                   ENDDO
     C*
     C*  LOAD THE WEEKS AND DAYS TO THE APPROPRIATE FIELDS
     C*
     C                   CLEAR                   X1
     C                   CLEAR                   X2
     C     WK(1)         IFEQ      0
     C                   MOVE      *BLANKS       CL40A6
     C                   ELSE
     C     WK(1)         IFLE      9
     C                   MOVE      WK(1)         X1                1
     C                   MOVEL     X1            CL40A6
     C                   ELSE
     C     WK(1)         IFLE      99
     C                   MOVE      WK(1)         X2                2
     C                   MOVEL     X2            CL40A6
     C                   ELSE
     C                   MOVEL     WK(1)         CL40A6
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     DY(1)         IFNE      *ZEROS
     C*** BEGIN CHG *** 11/10/99 **************************************
     C******************** MOVELDY,1      CL1A1
     C                   MOVE      DY(1)         CL1A1
     C*** END CHG ***** 11/10/99 **************************************
     C                   ELSE
     C                   MOVE      *BLANKS       CL1A1
     C                   ENDIF
     C*
     C**********           MOVELWK,2      CL40A7
     C     WK(2)         IFEQ      0
     C                   MOVE      *BLANKS       CL40A7
     C                   ELSE
     C     WK(2)         IFLE      9
     C                   MOVE      WK(2)         X1                1
     C                   MOVEL     X1            CL40A7
     C                   ELSE
     C     WK(2)         IFLE      99
     C                   MOVE      WK(2)         X2                2
     C                   MOVEL     X2            CL40A7
     C                   ELSE
     C                   MOVEL     WK(2)         CL40A7
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     DY(2)         IFNE      *ZEROS
     C*** BEGIN CHG *** 11/10/99 **************************************
     C******************** MOVELDY,2      CL1A2
     C                   MOVE      DY(2)         CL1A2
     C*** END CHG ***** 11/10/99 **************************************
     C                   ELSE
     C                   MOVE      *BLANKS       CL1A2
     C                   ENDIF
     C*
     C**********           MOVELWK,3      CL40A8
     C     WK(3)         IFEQ      0
     C                   MOVE      *BLANKS       CL40A8
     C                   ELSE
     C     WK(3)         IFLE      9
     C                   MOVE      WK(3)         X1                1
     C                   MOVEL     X1            CL40A8
     C                   ELSE
     C     WK(3)         IFLE      99
     C                   MOVE      WK(3)         X2                2
     C                   MOVEL     X2            CL40A8
     C                   ELSE
     C                   MOVEL     WK(3)         CL40A8
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     DY(3)         IFNE      *ZEROS
     C*** BEGIN CHG *** 11/10/99 **************************************
     C******************** MOVELDY,3      CL1A3
     C                   MOVE      DY(3)         CL1A3
     C*** END CHG ***** 11/10/99 **************************************
     C                   ELSE
     C                   MOVE      *BLANKS       CL1A3
     C                   ENDIF
     C*
     C**********           MOVELWK,4      CL40A9
     C     WK(4)         IFEQ      0
     C                   MOVE      *BLANKS       CL40A9
     C                   ELSE
     C     WK(4)         IFLE      9
     C                   MOVE      WK(4)         X1                1
     C                   MOVEL     X1            CL40A9
     C                   ELSE
     C     WK(4)         IFLE      99
     C                   MOVE      WK(4)         X2                2
     C                   MOVEL     X2            CL40A9
     C                   ELSE
     C                   MOVEL     WK(4)         CL40A9
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     DY(4)         IFNE      *ZEROS
     C*** BEGIN CHG *** 11/10/99 **************************************
     C******************** MOVELDY,4      CL1A4
     C                   MOVE      DY(4)         CL1A4
     C*** END CHG ***** 11/10/99 **************************************
     C                   ELSE
     C                   MOVE      *BLANKS       CL1A4
     C                   ENDIF
     C*
     C**********           MOVELWK,5      CL40A0
     C     WK(5)         IFEQ      0
     C                   MOVE      *BLANKS       CL40A0
     C                   ELSE
     C     WK(5)         IFLE      9
     C                   MOVE      WK(5)         X1                1
     C                   MOVEL     X1            CL40A0
     C                   ELSE
     C     WK(5)         IFLE      99
     C                   MOVE      WK(5)         X2                2
     C                   MOVEL     X2            CL40A0
     C                   ELSE
     C                   MOVEL     WK(5)         CL40A0
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
     C                   ENDSR
      *
