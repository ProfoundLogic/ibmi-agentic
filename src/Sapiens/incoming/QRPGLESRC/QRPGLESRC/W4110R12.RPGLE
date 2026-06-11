     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W4110R01 - FORM 28B
     H* ***************************************************************
     H* PROGRAM DESCRIPTION                                           *
     H*                                                               *
     H* This program prompts the user for additional input necessary  *
     H* to print a department letter.                                 *
     H*                                                               *
     H*  CALLED FROM:  W4110R                                         *
     H*                                                               *
     H*  CALLS TO:     NONE                                           *
     H*                                                               *
     H*  PROGRAMMER:   C:47                                           *
     H*                                                               *
     H*  CREATED:      12/10/96             PROJECT: 81WAW            *
     H*                                                               *
     H* ***************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/02/03 C:53  Project: 102CP # 39788                         *
     H*                Change fields to tie into base.                *
     H*                                                               *
     H* 09/22/03 C:53  Project: 102CP                                 *
     H*                ADD PROCESSING OF NC28BV2 LETTER FIELDS        *
     H*                                                               *
     H*  08/29/02  C:132 SEQ#: WC967
     H*                  Add new Pay Value "U" for settlement amounts *
     H*                  that include Attorney's Fees.                *
     H*                                                               *
     H* 07/06/01 C:53  Project: 81WSL                                 *
     H*                Add attny field to input.                      *
     H*                                                               *
     H* 03/26/01 C:15  Support: WC081/26847                           *
     H*                Corrected date loaded into CL20AJ.             *
     H*                                                               *
     H* 12/20/98  C:02  PROJECT: 81WAW                                *
     H*           Reimplementation into V3, additional input screens  *
     H*           for Claim Dept Letters - Document NC28BDOC          *
     H*                                                               *
     H* ***************************************************************
     FW4110F12  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FWDC2L     O    E             DISK
     FWDCLL     UF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMDEL     IF   E           K DISK
     FWMDOL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F*** BEG CHG *** 07/06/01 *****************************
     F***WDPAL5  IF  E           K        DISK
     FWDPALK    IF   E           K DISK
     F*** END CHG *** 07/06/01 *****************************
     FWMEML     IF   E           K DISK
     F*** BEG DEL *** 12/02/03 *****************************
     F**SMH1L   IF  E           K        DISK
     F*** END DEL *** 12/02/03 *****************************
     FWMWHL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)
      *

     D*** BEG CHG *** 07/06/01 *****************************
     D***********         WK          3  3 0             WEEKS
     D**********          DY          3  3 0             DAYS
     D WK              S              3  0 DIM(5)                               WEEKS
     D DY              S              3  0 DIM(5)                               DAYS
     D*** END CHG *** 07/06/01 *****************************
      *
     D*** BEG ADD *** 07/06/01 *****************************
     D                 DS
     D  WW2                    1      2
     D  WM1                    1      1
     D  WM2                    2      2
     D                 DS
     D  XXZIP                  1     10
     D  XXZ5                   1      5
     D  XXDASH                 6      6
     D  XXZ4                   7     10
     D*** END ADD *** 07/06/01 *****************************
     D                 DS
     D*** Begin dlt *** 03/26/01 **************************************
     D**************************************  1  120DSDTTM
     D**************************************  1   60DSTIME
     D**************************************  7  120DSDATE
     D*** End dlt ***** 03/26/01 **************************************
     D*** Begin add *** 03/26/01 **************************************
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*** End add ***** 03/26/01 **************************************
      *
     D DATEDS          DS
     D  CYMD                   1      8  0
     D  CC1                    1      2  0
     D  CY1                    1      4
     D  YY1                    3      4  0
     D  YMD                    3      8  0
     D  MMDD                   5      8  0
     D  MM                     5      6
     D  DD                     7      8
     D  CC2                    9     10  0
     D  CY2                    9     12
     D  YY2                   11     12  0
     D  MDCY                   5     12  0
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
     C*  FUND AUXILARY FILE
     C*
     C     F2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*  CUSTOM GROUP ID FILE - SMH1P
     C*
     C     H1KY          KLIST
     C                   KFLD                    XXKEY            40
     C                   KFLD                    XXCSID            2
     C                   KFLD                    XXRFMT            1
     C*
     C*  CLAIMS CHECK PAYMENT FILE
     C*
     C     PAKY          KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLFYR
     C                   KFLD                    CLFCLM
     C                   KFLD                    XXTRDT
     C*
     C     PAKY1         KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLFYR
     C                   KFLD                    CLFCLM
     C*
     C*  TABLE FILE - WTABL
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY2           10
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKY1         KLIST
     C                   KFLD                    CMCLEX
     C*
     C*   KEY LIST FOR WORK HISTORY FILE
     C*
     C     WHKY          KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLFYR
     C                   KFLD                    CLFCLM
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    CLFND         XXFND
     C     *LIKE         DEFINE    CLREQ#        XXREQ#
     C     *LIKE         DEFINE    DEDEPT        XXDEPT
     C     *LIKE         DEFINE    CMACDT        DATE1
     C     *LIKE         DEFINE    CMACDT        DATE2
     C     *LIKE         DEFINE    CMACDT        XXTRDT
     C     *LIKE         DEFINE    CMACDT        S1TRDT
     C     *LIKE         DEFINE    CMACDT        S2TRDT
     C     *LIKE         DEFINE    CMACDT        S3TRDT
     C     *LIKE         DEFINE    CMACDT        S4TRDT
     C     *LIKE         DEFINE    CMACDT        S5TRDT
     C     *LIKE         DEFINE    CMACDT        S6TRDT
     C     *LIKE         DEFINE    CL11N1        XX11N
     C     *LIKE         DEFINE    CL11N1        XX11N4
     C*** BEG ADD *** 07/06/01 ****************************************
     C     *LIKE         DEFINE    PATODT        SVTOD1
     C     *LIKE         DEFINE    PATODT        SVTOD2
     C*** END ADD *** 07/06/01 ****************************************
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
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVEL     CONAME        CLCNME
     C*** BEG ADD *** 07/06/01 *****************************
     C                   MOVE(P)   COFARA        XX3A
     C                   MOVEL(P)  COFAX#        XX3B
     C                   MOVE(P)   COFAX#        XX4A
     C     ' '           CAT       XX3A          CL20AE
     C     CL20AE        CAT       ' ':0         CL20AE
     C     CL20AE        CAT       XX3B:1        CL20AE
     C     CL20AE        CAT       '-':0         CL20AE
     C     CL20AE        CAT       XX4A:0        CL20AE
     C*** END ADD *** 07/06/01 *****************************
     C*** BEG ADD *** 12/02/03 ****************************
     C                   MOVEL     COZIP         XXZP1             5
     C                   MOVE      COZIP         XXZP2             4
     C     XXZP2         IFEQ      *ZEROS
     C                   MOVEL(P)  XXZP1         XXZIP            10
     C                   ELSE
     C     XXZP1         CAT(P)    '-':0         XXZIP
     C     XXZIP         CAT(P)    XXZP2:0       XXZIP
     C                   ENDIF
     C*
     C                   MOVEL(P)  CONAME        CLCNME
     C                   MOVEL     COADR1        CL60A1
     C     ' '           CAT(P)    COCTY         XX30A            30
     C     XX30A         CAT       ',':0         XX30A
     C     XX30A         CAT       COST:1        XX30A
     C     XX30A         CAT       XXZIP:1       XX30A
     C                   MOVE      XX30A         CL60A1                         CO ADR1+C/S/Z
     C*** END ADD *** 12/02/03 ****************************
     C                   END
     C*
     C*  GET THE FUND RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNNAME        SCNAME
     C                   END
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
     C                   MOVE      *BLANKS       SCIFM
     C*** BEG CHG *** 07/06/01 *****************************
     C**********           MOVE *BLANKS   SCRCC
     C                   MOVE      *BLANKS       SCMFP
     C                   MOVE      *BLANKS       SCCCFP
     C*** END CHG *** 07/06/01 *****************************
      *
     C                   ELSE
      *
     C     CL1A3         IFNE      *BLANK
     C                   MOVE      'Y'           SCIFM
     C                   ELSE
     C                   MOVE      *BLANK        SCIFM
     C                   END
      *
     C*** BEG DEL *** 07/06/01 *****************************
     C*********  CL1A7     IFNE *BLANK
     C*********            MOVE 'Y'       SCRCC
     C*********            ELSE
     C*********            MOVE *BLANK    SCRCC
     C*********            END
     C*** END DEL *** 07/06/01 *****************************
      *
     C                   END                                                     *IN50
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
     C     CDMID         IFNE      *BLANK
     C     SCCLNM        CAT       '.':0         SCCLNM
     C                   END
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
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
      *
      * RESET INDICATORS 30 - 32
      *
     C                   MOVEA     '000'         *IN(30)
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
      * ****************************************************************
      *   PROCESS REQUEST SCREEN 1
      * ****************************************************************
     C     SCR01T        BEGSR
      *
      * VALIDATE RESPONSES
      *
     C     *IN30         DOUEQ     *OFF
      *
     C     SCIFM         IFNE      'Y'
     C     SCIFM         ANDNE     'N'
     C                   MOVE      *ON           *IN30                          GEN ERROR IND
     C                   MOVE      *ON           *IN31
     C                   LEAVE
     C                   END
      *
     C*** BEG CHG *** 07/06/01 *****************************
     C********** SCRCC     IFNE 'Y'
     C********** SCRCC     ANDNE'N'
     C**********           MOVE *ON       *IN30            GEN ERROR IND
     C**********           MOVE *ON       *IN32
     C**********           LEAVE
     C**********           END
     C     SCMFP         IFNE      'Y'
     C     SCMFP         ANDNE     'N'
     C                   MOVE      *ON           *IN30                          GEN ERROR IND
     C                   MOVE      *ON           *IN32
     C                   LEAVE
     C                   END
     C     SCCCFP        IFNE      'Y'
     C     SCCCFP        ANDNE     'N'
     C                   MOVE      *ON           *IN30                          GEN ERROR IND
     C                   MOVE      *ON           *IN33
     C                   LEAVE
     C                   END
     C*** END CHG *** 07/06/01 *****************************
      *
      *  WE HAVE PASSED ALL EDITS, PROCESS SCREEN DATA
      *
     C     *IN30         IFEQ      *ON
     C                   MOVEL     'SCR01'       Q2RECF
     C                   ELSE
      *
     C     SCIFM         IFEQ      'Y'
     C                   MOVE      'X'           CL1A3
     C                   ELSE
     C                   MOVE      'X'           CL1A4
     C                   END
      *
     C*** BEG CHG *** 07/06/01 *****************************
     C********** SCRCC     IFEQ 'Y'
     C**********           MOVE 'X'       CL1A7
     C**********           ELSE
     C**********           MOVE 'X'       CL1A8
     C**********           END
     C     SCMFP         IFEQ      'Y'
     C                   MOVE      'X'           CL1A7
     C                   ELSE
     C                   MOVE      'X'           CL1A8
     C                   END
     C     SCCCFP        IFEQ      'Y'
     C                   MOVE      'X'           CL1A11
     C                   ELSE
     C                   MOVE      'X'           CL1A12
     C                   END
     C*** END CHG *** 07/06/01 *****************************
      *
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
      *  GET FUND AUXILARY FILE TO GET CARRIER #
      *
     C     F2KEY         CHAIN     PF201                              60
     C     *IN60         IFEQ      *OFF
     C*** BEG DEL *** 12/02/03 ****************************
     C***********          MOVELF2CRCD    CL20A3
     C*** END DEL *** 12/02/03 ****************************
     C*** BEG ADD *** 12/02/03 ****************************
     C                   MOVEL(P)  F2IAUD        CL20A3
     C*
     C                   MOVE      F2AREA        XXAREA            3
     C                   MOVEL     F2SPH#        XXSPH1            3
     C                   MOVE      F2SPH#        XXSPH2            4
     C     XXAREA        CAT(P)    XXSPH1:1      CL40A1
     C     CL40A1        CAT       '-':0         CL40A1
     C     CL40A1        CAT       XXSPH2:0      CL40A1                         FND PH#
     C*** END ADD *** 12/02/03 ****************************
     C                   END
     C                   MOVE      CMFND         X1FND             3            FUND
     C                   MOVE      CMFYR         X1FYR             3            FUND YEAR
     C                   MOVE      CMCASE        X1CASE            7            CASE #
     C     X1FND         CAT       '-':0         CL20A1
     C     CL20A1        CAT       X1FYR:0       CL20A1
     C     CL20A1        CAT       '-':0         CL20A1
     C     CL20A1        CAT       X1CASE:0      CL20A1                         CARRIER FILE#
      *
      *  CLAIMANT NAME
      *
     C                   MOVE      *BLANKS       XX37A            37            LAST, FIRST M.
     C     CDLSN         CAT       ',':0         XX37A
     C     XX37A         CAT       CDFSN:1       XX37A
     C     CDMID         IFNE      *BLANK
     C     XX37A         CAT       CDMID:1       XX37A
     C     XX37A         CAT       '.':0         XX37A
     C                   END
     C                   MOVEL(P)  XX37A         CL40A4                         CLAIMANT NAME
      *
     C     X1SS1         CAT       '-':0         CL20A2                         CLAIMANT SS#
     C     CL20A2        CAT       X1SS2:0       CL20A2
     C     CL20A2        CAT       '-':0         CL20A2
     C     CL20A2        CAT       X1SS3:0       CL20A2
      *****
     C                   MOVEL(P)  CDADR1        CL60A2
     C*****                MOVEL*BLANKS   XX42A  42        CLM CITY,ST,ZIP
     C*****                MOVEL' '       XX42A
     C*****      XX42A     CAT  CDCTY:1   XX42A
     C*****      XX42A     CAT  ',':0     XX42A
     C*****      XX42A     CAT  CDST:1    XX42A
     C*****                MOVELCDZIP     XX5A    5 P
     C*****                MOVE CDZIP     XX4A    4 P
     C*****      XX42A     CAT  XX5A:1    XX42A
     C*****      XX42A     CAT  '-':0     XX42A
     C*****      XX42A     CAT  XX4A:0    XX42A
     C*****                MOVE XX42A     CL60A2
      *
     C     CDPHN#        IFNE      *ZERO
     C                   MOVE(P)   CDAREA        XX3A              3
     C                   MOVEL(P)  CDPHN#        XX3B              3
     C                   MOVE(P)   CDPHN#        XX4A              4
     C*** BEG CHG *** 07/06/01 ****************************
     C*********  '('       CAT  XX3A:0    CL20A8           CLAIMANT PH#
     C*********  CL20A8    CAT  ')':0     CL20A8
     C     ' '           CAT       XX3A:0        CL20A8                         CLAIMANT PH#
     C     CL20A8        CAT       ' ':0         CL20A8
     C*** END CHG *** 07/06/01 ****************************
     C     CL20A8        CAT       XX3B:1        CL20A8
     C     CL20A8        CAT       '-':0         CL20A8
     C     CL20A8        CAT       XX4A:0        CL20A8
     C                   END
     C*** BEG ADD *** 07/06/01 ****************************
      *
     C                   MOVEL     *BLANKS       XX42A            42            CLM CITY,ST,ZIP
     C                   MOVEL     ' '           XX42A
     C     XX42A         CAT       CDCTY:1       XX42A
     C     XX42A         CAT       ',':0         XX42A
     C     XX42A         CAT       CDST:1        XX42A
     C                   MOVEL(P)  CDZIP         XX5A              5
     C                   MOVE(P)   CDZIP         XX4A              4
     C     XX42A         CAT       XX5A:1        XX42A
     C     XX42A         CAT       '-':0         XX42A
     C     XX42A         CAT       XX4A:0        XX42A
     C                   MOVEL(P)  XX42A         CL60A3
     C*
     C                   SELECT
     C     CDSEX         WHENEQ    'M'
     C                   MOVEL     ' X '         CL10A1
     C     CDSEX         WHENEQ    'F'
     C                   MOVE      'X   '        CL10A1
     C                   ENDSL
     C*
     C                   Z-ADD     CDDTOB        CYMD
     C                   EXSR      SRCDAT
     C     '/':' '       XLATE     XXDT10        CL10A4
     C                   MOVEL     CL10A4        WW2
     C                   MOVEL     '  '          CL10A4
     C                   MOVE      WM1           CL1A10
     C                   MOVEL     WM2           CL10A4
     C*
     C                   MOVE(P)   CDWARA        XX3A
     C                   MOVEL(P)  CDWPH#        XX3B
     C                   MOVE(P)   CDWPH#        XX4A
     C                   MOVEL(P)  XX3A          WW12             12
     C     WW12          CAT       ' ':0         WW12
     C     WW12          CAT       XX3B:1        WW12
     C     WW12          CAT       '-':0         WW12
     C     WW12          CAT       XX4A:0        WW12
     C                   MOVE      WW12          CL20AG
      *
     C*** END ADD *** 07/06/01 ****************************
      *
      *  CONVERT YMD DATES TO MDY - ACCIDENT DATE
      *
     C                   CLEAR                   DATEDS
     C                   Z-ADD     CMACDT        CYMD                            FROM  DATE
     C                   EXSR      SRCDAT
     C*** BEG CHG *** 07/06/01 ***************************
     C************         MOVELXXDT10    CL20AF    P
     C                   MOVEL(P)  XXDT10        CL10AF
     C*** END CHG *** 07/06/01 ***************************
      *
     C                   MOVEL     CMACL#        CL40A2                         IC FILE#   25A
      *
      *  SALARY CONTINUED?
      *
     C     CMFSAL        IFEQ      'Y'
     C                   MOVE      'X'           CL1A1
     C                   ELSE
     C                   MOVE      'X'           CL1A2
     C                   END
      *
      *  GET EMPLOYER INFO FROM EMPLOYER MASTER
      *
     C     EMKEY         CHAIN     PEM01                              62
     C     *IN62         IFEQ      *OFF
     C*** BEG DEL *** 07/06/01 ****************************
     C*********            MOVELEMSEMP    CL20A3           EMP CODE# 7,0
     C*********  29        SUBSTEMNAME    CL40A3           EMPLOYER NAME
     C*** END DEL *** 07/06/01 ****************************
     C*** BEG ADD *** 07/06/01 ****************************
     C*
     C                   MOVEL     EMSEMP        CL20A4                         EMP CODE# 7,0
     C                   MOVEL(P)  EMNAME        CL60A7                         EMPLOYER NAME
      *
     C                   MOVE(P)   EMAREA        XX3A              3
     C                   MOVEL(P)  EMPHN#        XX3B
     C                   MOVE(P)   EMPHN#        XX4A
     C*
     C     ' '           CAT(P)    XX3A:0        WW12                           EMP PH#
     C     WW12          CAT       ' ':0         WW12
     C     WW12          CAT       XX3B:1        WW12
     C     WW12          CAT       '-':0         WW12
     C     WW12          CAT       XX4A:0        WW12
     C                   MOVEL(P)  WW12          WW31             30
     C                   MOVE      WW31          CL60A7
      *
     C                   MOVE      *BLANKS       XX29A            29
     C                   MOVEL     EMMAD1        XX29A
     C                   MOVEL     XX29A         CLEMA1                         EMP ADDRESS
      *
     C                   MOVEL     *BLANKS       XX29A                          EMP CITY,ST,ZIP
     C     EMMCTY        CAT       ',':0         XX29A
     C     XX29A         CAT       EMMST:1       XX29A
     C                   MOVEL(P)  EMMZIP        XXZ5
     C                   MOVE(P)   EMMZIP        XXZ4
     C     XXZ4          IFNE      '0000'
     C                   MOVE      '-'           XXDASH
     C                   ELSE
     C                   MOVE      ' '           XXDASH
     C                   MOVE(P)   *BLANKS       XXZ4
     C                   ENDIF
     C     XX29A         CAT       XXZIP:1       XX29A
     C                   MOVE      XX29A         CLEMCS
     C                   MOVEL(P)  CLEMA1        CL60A8
     C                   MOVE      XX29A         CL60A8
     C*
     C*** END ADD *** 07/06/01 ****************************
      *
     C                   MOVE      *BLANKS       XX29A            29
     C                   MOVEL     EMMAD1        XX29A
     C                   MOVEL     XX29A         CLEMA1                         EMP ADDRESS
      *
     C                   MOVEL     *BLANKS       XX29A                          EMP CITY,ST,ZIP
     C     EMMCTY        CAT       ',':0         XX29A
     C     XX29A         CAT       EMMST:1       XX29A
     C                   MOVE(P)   EMMZIP        XX9A              9
     C     XX29A         CAT       XX9A:1        XX29A
     C                   MOVE      XX29A         CLEMCS
      *
     C                   MOVE(P)   EMAREA        XX3A              3
     C                   MOVEL(P)  EMPHN#        XX3B
     C                   MOVE(P)   EMPHN#        XX4A
     C     '('           CAT       XX3A:0        CL20A0                         EMP PH#
     C     CL20A0        CAT       ')':0         CL20A0
     C     CL20A0        CAT       XX3B:1        CL20A0
     C     CL20A0        CAT       '-':0         CL20A0
     C     CL20A0        CAT       XX4A:0        CL20A0
     C                   END
     C*** BEG DEL *** 12/02/03 ****************************
      **********
      ***GET*THE CUSTOM GROUP ID
      **********
     C**********           MOVE XXCO#     XX3A    3 P
     C**********           MOVE XXFND     XX3B    3 P
     C********** XX3A      CAT  XX3B:0    XXKEY     P
     C**********           MOVEL'CA'      XXCSID
     C**********           MOVEL'B'       XXRFMT
     C********** H1KY      CHAINPH101                60
     C********** *IN60     IFEQ *OFF
     C**********           MOVELH1UB01    CL60A1
     C**********           MOVE H1UB02    XX36A  36 P      BLANK IN FRONT
     C**********           MOVE XX36A     CL60A1           CO ADR1+C/S/Z
     C**********           MOVELH1UB03    CL40A1           CO PH#
     C**********           END
     C*** END DEL *** 12/02/03 ****************************
      *
      *  GET THE PAYMENT DETAIL RECORD
      *
     C                   Z-ADD     *ZERO         XXTRDT                         TRANS DATE
     C                   MOVE      'N'           FIRST1            1
     C                   MOVE      'N'           FIRST2            1
     C                   MOVE      'N'           FIRST3            1
     C     PAKY          SETLL     PPA01
      *
     C     *IN61         DOUEQ     *ON
     C     PAKY1         READE     PPA01                                  61
     C     *IN61         IFEQ      *OFF
     C*
     C*   CALCULATE WEEKS & DAYS
     C*
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'Z'
     C     PATYPE        OREQ      'A'
     C     PAADST        ANDEQ     *BLANKS
     C     PATYPE        OREQ      'M'
     C     PAAMT         ANDGE     0
     C     PATYPE        OREQ      'A'
     C     PAADST        ANDEQ     'A'
     C     PAAMT         ANDGT     0
     C                   ELSE
     C                   MULT      -1            PAWKS
     C                   MULT      -1            PADAYS
     C                   END
     C*
     C*  GET THE PMT VAL 1 PLACEMENT VALUE TO DETERMINE WHICH
     C*  CATEGORY THE AMOUNT WILL BE ADDED TO.
     C*
     C                   MOVEL(P)  PABR2         XXKEY2
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
      *
     C     PARETP        IFEQ      'C'                                          COMP ONLY
      *
      *  CALCULATE TOTAL # OF WEEKS FOR TEMPORARY TOTAL DISABILITY
      *
     C     B2PLCD        IFEQ      '1'                                          #WKS TEMP TOT
     C                   ADD       PAAMT         XXTT             11 2
     C                   ADD       PAWKS         WK(1)
     C                   ADD       PADAYS        DY(1)
      *
      * DON'T INCLUDE VOIDS, STOPS, OR REVERSALS IN DATE RANGE.
      *
     C     PATYPE        IFNE      'V'
     C     PATYPE        ANDNE     'S'
     C     PATYPE        ANDNE     'Z'
     C     PAADST        ANDNE     'V'
     C     PAADST        ANDNE     'S'
      *
      *  CONVERT YMD DATES TO MDY
      *
     C     NXT1          TAG                                                    ADD 07/06/01
      *
     C     FIRST1        IFEQ      'N'
     C                   CLEAR                   DATEDS
     C                   Z-ADD     PAFRDT        CYMD                           FROM  DATE
     C                   EXSR      SRCDAT
     C*** BEG CHG *** 07/06/01 ***************************
     C************         MOVELXXDT10    CL20A5    P
     C                   MOVEL(P)  XXDT10        CL10A5
     C*** END CHG *** 07/06/01 ***************************
      *
     C                   CLEAR                   DATEDS
     C                   Z-ADD     PATODT        CYMD                           TO DATE
     C                   EXSR      SRCDAT
     C*** BEG CHG *** 07/06/01 ***************************
     C************         MOVELXXDT10    CL20A6    P
     C                   MOVEL(P)  XXDT10        CL10A6
     C*** END CHG *** 07/06/01 ***************************
      *
     C                   Z-ADD     PAFRDT        S1TRDT                         FROM DATE
     C                   Z-ADD     PATODT        S2TRDT                         TO DATE
     C                   MOVE      'Y'           FIRST1
     C*** BEG ADD *** 07/06/01 ***************************
     C*
     C                   ELSE
     C*
     C** TRANSLATE LAST TO DATE AND ADD 1
     C*
     C                   Z-ADD     SVTOD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C*
     C                   Z-ADD     1             SRFUTR            5 0
     C                   MOVE      *BLANKS       SRRTCD            1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C*
     C     SRRTCD        IFEQ      *OFF
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SVTOD1                          TO DT +1
     C                   ENDIF
     C*
     C** COMPARE FROM DATE TO LAST TO DATE +1
     C***
     C**N21      PAFRDT    IFNE SVTOD1
     C***                  MOVE *ON       *IN21
     C***                  MOVE 'N'       FIRST1
     C***                  GOTO NXT1
     C***                  ENDIF
     C*
     C*** END ADD *** 07/06/01 ***************************
     C                   END                                                    FIRST1
     C*
     C*
     C     PAFRDT        IFLT      S1TRDT
     C                   CLEAR                   DATEDS
     C                   Z-ADD     PAFRDT        CYMD                           FROM DATE
     C                   EXSR      SRCDAT
     C*** BEG CHG *** 07/06/01 ***************************
     C************         MOVELXXDT10    CL20A5    P
     C                   MOVEL(P)  XXDT10        CL10A5
     C*** END CHG *** 07/06/01 ***************************
     C                   Z-ADD     PAFRDT        S1TRDT                         FROM DATE
     C                   END
      *
     C     PATODT        IFGT      S2TRDT
     C                   CLEAR                   DATEDS
     C                   Z-ADD     PATODT        CYMD                           TO DATE
     C                   EXSR      SRCDAT
     C*** BEG CHG *** 07/06/01 ***************************
     C************         MOVELXXDT10    CL20A6    P
     C                   MOVEL(P)  XXDT10        CL10A6
     C*** END CHG *** 07/06/01 ***************************
     C                   Z-ADD     PATODT        S2TRDT                         TO DATE
     C                   END
      *
     C                   END                                                    PATYPE<>V,S,Z
     C                   END                                                    B2PLCD = 1
      *
      *  CALCULATE TOTAL # OF WEEKS FOR TEMPORARY PARTIAL DISABILITY
      *
     C     B2PLCD        IFEQ      '7'                                          TEMP PART
     C                   ADD       PAAMT         XXTP             11 2
     C                   ADD       PAWKS         WK(2)
     C                   ADD       PADAYS        DY(2)
      *
      * DON'T INCLUDE VOIDS, STOPS, OR REVERSALS IN DATE RANGE.
      *
     C     PATYPE        IFNE      'V'
     C     PATYPE        ANDNE     'S'
     C     PATYPE        ANDNE     'Z'
     C     PAADST        ANDNE     'V'
     C     PAADST        ANDNE     'S'
      *
      *  CONVERT YMD DATES TO MDY
      *
     C     NXT2          TAG                                                    ADD 07/06/01
      *
     C     FIRST2        IFEQ      'N'
     C                   CLEAR                   DATEDS
     C                   Z-ADD     PAFRDT        CYMD                           FROM  DATE
     C                   EXSR      SRCDAT
     C*** BEG CHG *** 07/06/01 ***************************
     C************         MOVELXXDT10    CL20A7    P
     C                   MOVEL(P)  XXDT10        CL10A7
     C*** END CHG *** 07/06/01 ***************************
      *
     C                   CLEAR                   DATEDS
     C                   Z-ADD     PATODT        CYMD                           TO DATE
     C                   EXSR      SRCDAT
     C*** BEG CHG *** 07/06/01 ***************************
     C************         MOVELXXDT10    CL20A9    P
     C                   MOVEL(P)  XXDT10        CL10A9
     C*** END CHG *** 07/06/01 ***************************
      *
     C                   Z-ADD     PAFRDT        S3TRDT                         FROM DATE
     C                   Z-ADD     PATODT        S4TRDT                         TO DATE
     C                   MOVE      'Y'           FIRST2
     C                   END
     C*
      *
     C     PAFRDT        IFLT      S3TRDT
     C                   CLEAR                   DATEDS
     C                   Z-ADD     PAFRDT        CYMD                           FROM DATE
     C                   EXSR      SRCDAT
     C*** BEG CHG *** 07/06/01 ***************************
     C************         MOVELXXDT10    CL20A7    P
     C                   MOVEL(P)  XXDT10        CL10A7
     C*** END CHG *** 07/06/01 ***************************
     C                   Z-ADD     PAFRDT        S3TRDT                         FROM DATE
     C                   END
      *
     C     PATODT        IFGT      S4TRDT
     C                   CLEAR                   DATEDS
     C                   Z-ADD     PATODT        CYMD                           TO DATE
     C                   EXSR      SRCDAT
     C*** BEG CHG *** 07/06/01 ***************************
     C************         MOVELXXDT10    CL20A9    P
     C                   MOVEL(P)  XXDT10        CL10A9
     C*** END CHG *** 07/06/01 ***************************
     C                   Z-ADD     PATODT        S4TRDT                         TO DATE
     C                   END
      *
     C                   END                                                    PATYPE<>V,S,Z
     C                   END                                                    B2PLCD = 7
      *
      *  CALCULATE TOTAL # OF WEEKS FOR PERMANENT PARTIAL DISABILITY
      *
     C     B2PLCD        IFEQ      '9'                                          PERM PART
     C                   ADD       PAAMT         XXPP             11 2
     C                   ADD       PAWKS         WK(3)                          #WKS
     C                   ADD       PADAYS        DY(3)
      *
      * DON'T INCLUDE VOIDS, STOPS, OR REVERSALS IN DATE RANGE.
      *
     C     PATYPE        IFNE      'V'
     C     PATYPE        ANDNE     'S'
     C     PATYPE        ANDNE     'Z'
     C     PAADST        ANDNE     'V'
     C     PAADST        ANDNE     'S'
      *
      *  CONVERT YMD DATES TO MDY
      *
     C     FIRST3        IFEQ      'N'
     C                   CLEAR                   DATEDS
     C                   Z-ADD     PAFRDT        CYMD                           FROM  DATE
     C                   EXSR      SRCDAT
     C*** BEG CHG *** 07/06/01 ***************************
     C************         MOVELXXDT10    CL20AD    P
     C                   MOVEL(P)  XXDT10        CL10AD
     C*** END CHG *** 07/06/01 ***************************
      *
     C                   CLEAR                   DATEDS
     C                   Z-ADD     PATODT        CYMD                           TO DATE
     C                   EXSR      SRCDAT
     C*** BEG CHG *** 07/06/01 ***************************
     C************         MOVELXXDT10    CL20AE    P
     C                   MOVEL(P)  XXDT10        CL10AE
     C*** END CHG *** 07/06/01 ***************************
      *
     C                   Z-ADD     PAFRDT        S5TRDT
     C                   Z-ADD     PATODT        S6TRDT
     C                   MOVE      'Y'           FIRST3
     C                   END
      *
     C     PAFRDT        IFLT      S5TRDT
     C                   CLEAR                   DATEDS
     C                   Z-ADD     PAFRDT        CYMD                           FROM DATE
     C                   EXSR      SRCDAT
     C*** BEG CHG *** 07/06/01 ***************************
     C************         MOVELXXDT10    CL20AD    P
     C                   MOVEL(P)  XXDT10        CL10AD
     C*** END CHG *** 07/06/01 ***************************
     C                   Z-ADD     PAFRDT        S5TRDT
     C                   END
      *
     C     PATODT        IFGT      S6TRDT
     C                   CLEAR                   DATEDS
     C                   Z-ADD     PATODT        CYMD                           TO DATE
     C                   EXSR      SRCDAT
     C*** BEG CHG *** 07/06/01 ***************************
     C************         MOVELXXDT10    CL20AE    P
     C                   MOVEL(P)  XXDT10        CL10AE
     C*** END CHG *** 07/06/01 ***************************
     C                   Z-ADD     PATODT        S6TRDT
     C                   END
      *
     C                   END                                                    PATYPE<>V,S,Z
     C                   END                                                    B2PLCD = 9
     C     B2PLCD        IFEQ      *BLANK
     C                   ADD       PAAMT         XX11N4
     C                   END
      *
     C                   END                                                    PARETP = C
      *
     C     B2PLCD        IFEQ      'V'                                          DISFIGUREMENT
     C                   ADD       PAAMT         CL11N1
     C                   END
      *
     C     B2PLCD        IFEQ      '6'                                          DEATH
     C                   ADD       PAAMT         CL11N2
     C                   END
      *
     C     B2PLCD        IFEQ      'W'                                          LOSS OF ORGAN
     C                   ADD       PAAMT         CL11N3
     C                   END
      *
     C     B2PLCD        IFEQ      'M'                                          ATTORNEY FEE
     C                   ADD       PAAMT         XX11N
     C                   END
      *
     C     B2PLCD        IFEQ      'L'                                          COMPROMISE/SETL
      ***** BEG ADD **** 08/29/02 ****************************************
     C     B2PLCD        OREQ      'U'                                          COMPROMISE/SETL
      ***** END ADD **** 08/29/02 ****************************************
     C                   ADD       PAAMT         CL11N5
     C                   END
      *
     C     PARETP        IFEQ      'M'                                          TOTAL MED PAID
     C     CMLTFL        ANDEQ     'I'
     C     B2PLCD        ANDNE     'I'
     C                   ADD       PAAMT         CL11N6
     C                   END
      *
     C     PARETP        IFEQ      'M'                                          TOTAL REHAB PD
     C     CMLTFL        ANDEQ     'I'
     C     B2PLCD        ANDEQ     'I'
     C                   ADD       PAAMT         CL11N7
     C                   END
      *
     C     PARETP        IFEQ      'M'                                          TOTAL MED ONLY
     C     CMLTFL        ANDEQ     'M'
     C                   ADD       PAAMT         CL11N8
     C                   END
      *
     C     B2PLCD        IFEQ      'G'                                          FUNERAL
     C                   ADD       PAAMT         CL11N0
     C                   END
      *
     C     B2PLCD        IFEQ      'P'                                          SECOND INJURY
     C                   ADD       PAAMT         CL11NA
     C                   END
      *
     C     B2PLCD        IFEQ      'X'                                          HEARING COSTS
     C                   ADD       PAAMT         CL11NB
     C                   END
      *
     C     B2PLCD        IFEQ      'Y'                                          EXPERT WITNESS
     C                   ADD       PAAMT         CL11NC
     C                   END
      *
     C     B2PLCD        IFEQ      'K'                                          OTHER
     C                   ADD       PAAMT         CL11ND
     C                   END
      *
     C                   END                                                    *IN61
     C                   ENDDO                                                  *IN61
      *
      *  LINES 3+4+5+6+7+8+ATTORNEY FEE
      *
     C                   Z-ADD     XXTT          CL11N4
     C                   ADD       XXTP          CL11N4
     C                   ADD       XXPP          CL11N4
     C                   ADD       CL11N1        CL11N4
     C                   ADD       CL11N2        CL11N4
     C                   ADD       CL11N3        CL11N4
     C                   ADD       XX11N         CL11N4
     C                   ADD       XX11N4        CL11N4
      *
      *  TOTAL LINES 9+10+11A+11B
      *
     C                   Z-ADD     CL11N4        CL11N9
     C                   ADD       CL11N5        CL11N9
     C                   ADD       CL11N6        CL11N9
     C                   ADD       CL11N7        CL11N9
      *
      *  TOTAL MISCELLANEOUS PAYMENTS
      *
     C                   Z-ADD     CL11N0        CL11NE
     C                   ADD       CL11NA        CL11NE
     C                   ADD       CL11NB        CL11NE
     C                   ADD       CL11NC        CL11NE
     C                   ADD       CL11ND        CL11NE
      *
      *  RETURNED TO WORK ALGORITHM - GET THE MOST CURRENT RECORD
      *  1. GET WORK HISTORY RECORD
      *
     C                   Z-ADD     CMRTW         XXTRDT
     C     WHKY          SETGT     PWH01
     C     WHKY          READPE    PWH01                                  62
     C     *IN62         DOWEQ     *OFF
      *
     C     WHWKST        IFEQ      'R'
     C                   Z-ADD     WHDATE        XXTRDT
     C                   LEAVE
     C                   END
      *
     C     WHKY          READPE    PWH01                                  62
     C                   ENDDO
      *
      *  4. HAS EMPLOYEE RETURNED TO WORK?
      *
     C     XXTRDT        IFNE      *ZERO
     C                   MOVE      'X'           CL1A5                          YES
     C                   ELSE
     C                   MOVE      'X'           CL1A6                          NO
     C                   END
      *
      *  5. GET AWW RECORD IF EMPLOYEE RETURNED TO WORK
      *
     C                   Z-ADD     *ZERO         CL9N2
     C     XXTRDT        IFNE      *ZERO
     C                   Z-ADD     CMAWW         CL9N2
      *
     C                   CLEAR                   DATEDS
     C                   Z-ADD     XXTRDT        CYMD                           RTW DATE
     C                   EXSR      SRCDAT
     C*** BEG CHG *** 07/06/01 ***************************
     C************         MOVELXXDT10    CL20AG    P
     C                   MOVEL(P)  XXDT10        CL10AG
     C*** END CHG *** 07/06/01 ***************************
     C                   END                                                    XXTRDT<>0
      *
      *  DATE OF LAST COMP CHECK
      *
     C     PAKY1         SETGT     PPA01
      *
     C     *IN65         DOUEQ     *ON
     C     PAKY1         READPE    PPA01                                  65
     C     *IN65         IFEQ      *OFF
     C     PARETP        IFEQ      'C'
     C                   CLEAR                   DATEDS
     C                   Z-ADD     PATRDT        CYMD
     C                   EXSR      SRCDAT
     C*** BEG CHG *** 07/06/01 ***************************
     C************         MOVELXXDT10    CL20AH    P      121396
     C                   MOVEL(P)  XXDT10        CL10AH
     C*** END CHG *** 07/06/01 ***************************
     C                   LEAVE
     C                   END
     C                   END
     C                   ENDDO
      *
      *  DATE OF LAST MEDICAL CHECK
      *
     C     PAKY1         SETGT     PPA01
      *
     C     *IN66         DOUEQ     *ON
     C     PAKY1         READPE    PPA01                                  66
     C     *IN66         IFEQ      *OFF
     C     PARETP        IFEQ      'M'
     C                   CLEAR                   DATEDS
     C                   Z-ADD     PATRDT        CYMD
     C                   EXSR      SRCDAT
     C*** BEG CHG *** 07/06/01 ***************************
     C************         MOVELXXDT10    CL20AI    P
     C                   MOVEL(P)  XXDT10        CL10AI
     C*** END CHG *** 07/06/01 ***************************
     C                   LEAVE
     C                   END
     C                   END
     C                   ENDDO
      *
     C*
     C*  CONVERT THE DAYS INTO WEEKS
     C*
     C                   DO        3             X                 3 0
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
     C                   MOVE      WK(1)         CLDT2                          TEMP TOT
     C                   MOVE      WK(2)         CLDT5                          TEMP PART
     C                   MOVE      WK(3)         CLDT8                          PERM PART
     C                   MOVE      WK(1)         ##W               3            TEMP TOT
     C                   MOVE      DY(1)         ##D               1
     C     ##D           CAT       '/7':0        #D2               3
     C     ##W           CAT       #D2:1         CL40AA
     C*
     C                   MOVE      WK(2)         ##W               3            TEMP TOT
     C                   MOVE      DY(2)         ##D               1
     C     ##D           CAT       '/7':0        #D2               3
     C     ##W           CAT       #D2:1         CL40AB
     C*
     C                   MOVE      WK(3)         ##W               3            PERM PAAART
     C                   MOVE      DY(3)         ##D               1
     C     ##D           CAT       '/7':0        #D2               3
     C     ##W           CAT       #D2:1         CL40AC
     C*
     C                   MOVEL     UPUSR         CL20AC                         REQUESTOR
      *
     C     UPKY1         CHAIN     PUP02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     UPUSNM        CL20AA                         ADJUSTOR NAME
     C                   MOVEL     UPTITL        CL20AB                         USER TITLE
     C                   END
     C*
      *
      *  PRINT CURRENT DATE
      *
     C*** Begin dlt *** 03/26/01 **************************************
     C*******************  CLEARDATEDS
     C*******************  Z-ADD*DATE     MDCY
     C*******************  MOVE *ZERO     CC1
     C*******************  MOVE UYEAR     YY1
     C*******************  EXSR SRCDAT
     C*******************  MOVELXXDT10    CL20AJ    P
     C*** End dlt ***** 03/26/01 **************************************
     C*** Begin add *** 03/26/01 **************************************
     C                   CLEAR                   DATEDS
     C                   Z-ADD     DSDATE        MDCY
     C*** BEG CHG *** 07/06/01 ***************************
     C                   MOVEL(P)  MM            CL10AJ
     C                   CAT       '/':0         CL10AJ
     C                   CAT       DD:0          CL10AJ
     C                   CAT       '/':0         CL10AJ
     C                   CAT       CY2:0         CL10AJ
     C***********          MOVELMM        CL20AJ    P
     C***********          CAT  '/':0     CL20AJ
     C***********          CAT  DD:0      CL20AJ
     C***********          CAT  '/':0     CL20AJ
     C***********          CAT  CY2:0     CL20AJ
     C*** END CHG *** 07/06/01 ***************************
     C*** End add ***** 03/26/01 **************************************
      *
     C     *IN50         IFEQ      *OFF
     C                   UPDATE    PCL01
     C                   WRITE     PC201
     C                   END
      *
     C                   ENDSR
      *
      * ===============================================================
      *   CONVERT 7,0 DATE TO 8,0 ;   0961213 --> 19961213
      *   THEN, CYMD TO MDCY      ;  19961213 --> 12/13/1996
      * ===============================================================
     C     SRCDAT        BEGSR
      *
     C     YMD           IFNE      *ZERO
     C     CC1           IFEQ      0
     C                   MOVE      '19'          CC1
     C                   ELSE
     C                   MOVE      '20'          CC1
     C                   END
      *
     C                   MOVE(P)   CY1           CY2
     C     MM            CAT(P)    '/':0         XXDT10           10
     C     XXDT10        CAT       DD:0          XXDT10
     C     XXDT10        CAT       '/':0         XXDT10
     C     XXDT10        CAT       CY1:0         XXDT10
     C                   END
      *
     C                   ENDSR
