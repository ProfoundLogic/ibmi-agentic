     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H/TITLE N0163R - A/R INQUIRY - SCREEN PROGRAM
     H*****************************************************************
     H**   (C) COPYRIGHT PROGRESSIVE DATA SOLUTIONS, INC. 1997
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** REVISIONS:                                                  **
     H*                                                               *
     H*  06/09/20  C:212 Project : SY420                              *
     H*                  Mask SSN                                     *
     H**                                                             **
     H** 01/07/99  C:94  PROJECT: MC152                              **
     H**                 CHANGE LENGTH OF INV# FROM 5 TO 9           **
     H**                                                             **
     H** 10/15/92  C:06  PROJECT: 99AAD                              **
     H**                 1. ADD SUBCRIBER SEARCH                     **
     H**                 2. CHANGED CODE TO PROPERLY DISPLAY TO      **
     H**                    A/R SUMMARY RECORDS ON SCR02.            **
     H**                 3. CHANGED THE ROLLUP FUNCTION TO DISPLAY   **
     H**                    THE "+" ONLY WHEN ADDITIONAL RCDS EXIST. **
     H**                                                             **
     H** 11/08/93  C:20  REQ/PROJ: ARMNT                             **
     H**                 1. ADD DISPLAY OF SUB# TO SCR02             **
     H**                 2. ACCEPT SUB#/GRP# FROM SCREEN FIELDS NOT  **
     H**                    USER APRMS FIELDS                        **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** INDICATOR USAGE:                                            **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     FSMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCMLA    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FFMSML     IF   E           K DISK
     FNMARL7    IF   E           K DISK
     F*
     FN0163F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR02S1:REC1)
     F                                     SFILE(SCR02S2:REC2)
     F                                     INFDS(FIDS01)

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  SCFRDT                 1      8  0
     D  XXDY                   3      4  0
     D                 DS
     D  XXDTTM                 1     12  0
     D  XXDT                   1      6  0
     D  XXTIME                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   COMPANY FILE                   (SMCOL)
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   SUBSCRIBER FILE                (NMSML)
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#            5 0          ADD 11/08/93
     C*
     C*  USER PARAMETER FILE             (FMUPL)
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  CLAIM DEMOGRAPHICS FILE         (FMCDL)
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*  CLAIM MASTER FILE               (FMCML1)
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#                         ADD 11/08/93
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     CMKEY2        KLIST
     C                   KFLD                    XXSS#
     C                   KFLD                    XXACDT
     C*
     C*  A/R DETAIL FILE                 (NMARL7)
     C*
     C     ARKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#                         ADD 11/08/93
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     ARKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#                         ADD 11/08/93
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXINV#
     C                   KFLD                    XXFRDT
     C*
     C     *LIKE         DEFINE    CMDT          XXFRDT
     C     *LIKE         DEFINE    CMDT          XXACDT
     C     *LIKE         DEFINE    CMDT          XXTODT
     C     *LIKE         DEFINE    CMDT          HLINDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C     TOP           TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   END
     C*
     C*  GET COMPANY NAME
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCCONM
     C                   ELSE
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   END
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C     QQCALL        COMP      '2'                                    08    CALLED BY PGM
     C*
     C***  LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   Z-ADD     *ZEROS        SCFYR
     C                   Z-ADD     *ZEROS        SCCASE
     C                   Z-ADD     *ZEROS        SCFRDT
     C                   Z-ADD     *ZEROS        SCTODT
     C*
     C***  ADD NEXT LINE *** 8/04/89 *** CONNIE/I0005 ***
     C*
     C                   Z-ADD     UPSUB#        SCSUB                          ADD 11/08/93
     C                   Z-ADD     UPFND         SCFND
     C*
     C*
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
     C*
     C                   MOVE      SCTODT        SCFRDT
     C                   MOVE      01            XXDY
     C                   MOVE      '0'           INIT              1
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C***  DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C1'
     C     Q2RECF        OREQ      'SCR02C2'
     C                   MOVEA     '111'         *IN(1)
     C  N04              WRITE     SCR02C1
     C  N04              WRITE     SCR99
     C                   EXFMT     SCR02C2
     C                   MOVEA     '0000'        *IN(1)
     C                   END                                                    ENDEQ SCR02
     C*****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*************************************
     C*   F3 = EXIT                       *
     C*************************************
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END                                                    ENDIF *INKC
     C*************************************
     C*   F5 = RESTART                    *
     C*************************************
     C*** IF RESTART IS REQUESTED, CLEAR SCR01 FIELDS FIRST
     C*
     C     *INKE         IFEQ      '1'
     C*
     C                   MOVEA     '111'         *IN(21)
     C                   WRITE     SCR02C1                                      CLEAR SUBFILE 1
     C                   WRITE     SCR02C2                                      CLEAR SUBFILE 2
     C                   MOVEA     '000'         *IN(21)
     C*
     C                   MOVE      '0'           INIT
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
     C*
     C                   MOVE      SCTODT        SCFRDT
     C                   MOVE      01            XXDY
     C                   GOTO      STRT
     C                   END                                                    ENDIF *INKE
     C**********************************
     C*   COMMAND 06 = SUB SEARCH      *
     C**********************************
     C     *INKF         IFEQ      '1'
     C                   CALL      'F0545R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCSUB
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQRTC         OREQ      '*'
     C     SCSUB         OREQ      *ZERO
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END B/*/0
     C                   END                                                    END *INKF
     C*************************************
     C*   F7 = GROUP SEARCH               *
     C*************************************
     C*
     C     *INKG         IFEQ      '1'
     C*
     C                   CALL      'F0508R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    SCSUB
     C                   PARM                    SCFND
     C                   PARM                    QQRTCD            1
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR98
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCFND         OREQ      *ZERO
     C     QQRTCD        OREQ      '*'
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    ENDIF *INKG
     C*************************************
     C*   F8 = CASE # SEARCH              *
     C*************************************
     C*
     C     *INKH         IFEQ      '1'
     C*
     C                   MOVE      *BLANKS       XXSS#
     C                   MOVE      SCFND         XXFND
     C                   CALL      'F0570R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCFYR
     C                   PARM                    SCCASE
     C                   PARM                    XXSS#
     C                   PARM                    QQRTCD
     C                   PARM                    XXFND             3 0          REQ. FOR SRC
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR98
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCFYR         OREQ      *ZERO
     C                   GOTO      STRT
     C                   END                                                    ENDIF 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   GOTO      SCR01T
     C                   END                                                    ENDIF '*'
     C*
     C                   END                                                    ENDIF *INKH
     C*************************************
     C*  COMMAND 9 - ALPHA SEARCH         *
     C*************************************
     C*
     C     *INKI         IFEQ      '1'
     C*
     C                   MOVE      *BLANKS       XXSS#             9
     C                   CALL      'F0578R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXSS#
     C                   PARM                    QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR98
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     XXSS#         OREQ      *ZERO
     C                   GOTO      STRT
     C                   END                                                    ENDIF 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      SCFND         XXFND
     C                   CALL      'F0571R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXSS#
     C                   PARM                    SCFYR
     C                   PARM                    SCCASE
     C                   PARM                    XXFND             3 0          REQ. FOR SRC
     C                   PARM                    QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR98
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     XXSS#         OREQ      *BLANK
     C                   GOTO      STRT
     C                   END                                                    ENDIF 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   GOTO      SCR01T
     C                   END                                                    ENDIF '*'
     C*
     C                   END                                                    ENDIF '*'
     C*
     C                   END                                                    ENDIF *INKI
     C*************************************
     C*  COMMAND 11 - SS#/ACC DATE SEARCH *
     C*************************************
     C     *INKK         IFEQ      '1'
     C*
     C                   MOVE      *BLANK        XXSS#             9
     C                   Z-ADD     *ZEROS        X1ACDT            8 0
     C                   CALL      'F0573R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXSS#
     C                   PARM                    X1ACDT
     C                   PARM                    QQRTCD            1
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR98
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     XXSS#         OREQ      *BLANK
     C                   GOTO      STRT
     C                   END                                                    ENDIF 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C*
     C                   Z-ADD     X1ACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C*
     C     CMKEY2        CHAIN     PCM02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMSUB#        SCSUB
     C                   MOVE      CMFND         SCFND
     C                   MOVE      CMFYR         SCFYR
     C                   MOVE      CMCASE        SCCASE
     C                   GOTO      SCR01T
     C                   END                                                    ENDIF *IN60
     C                   END                                                    ENDIF '*'
     C*
     C                   END                                                    ENDIF *INKK
     C*
     C*************************************
     C*   F12 = PREVIOUS                  *
     C*************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   GOTO      TERM
     C                   END                                                    ENDIF SCR01
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C1'
     C     Q2RECF        OREQ      'SCR02C2'
     C*
     C                   MOVEA     '111'         *IN(21)
     C                   WRITE     SCR02C1                                      CLEAR SUBFILE 1
     C                   WRITE     SCR02C2                                      CLEAR SUBFILE 2
     C                   MOVEA     '000'         *IN(21)
     C*
     C                   MOVE      '0'           INIT
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDIF SCR02
     C*
     C                   END                                                    ENDIF *INKL
     C*************************************
     C*   COMMAND 13 = CHANGE USER PARMS  *
     C*************************************
     C     *INKM         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'F0006R'
     C                   WRITE     SCR98
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C                   GOTO      TOP
     C                   END
     C*
     C                   END                                                    ENDIF *INKM
     C*************************************
     C*   *IN33 - ROLL UP REQUESTED       *
     C*************************************
     C     *IN33         IFEQ      '1'
     C                   EXSR      ARDETL
     C                   MOVE      '1'           *IN04
     C                   MOVE      '0'           *IN33
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02C2'     Q2RECF
     C                   GOTO      STRT
     C                   END
     C****
     C                   GOTO      STRT
     C                   END                                                    END CMDKEY
     C*****************************************************************
     C*
     C***  GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR02C1'     SCR02T
     C     Q2RECF        CABEQ     'SCR02C2'     SCR02T
     C**************************************
     C*    PROCESS REQUEST SCREEN - SCR01  *
     C**************************************
     C     SCR01T        TAG
     C*
     C                   Z-ADD     SCSUB         XXSUB#                         ADD 11/08/93
     C*
     C*** SUBSCRIBER VALIDATION
     C*
     C     SMKEY         CHAIN     PSM01                              25        INVALID SUBS
     C     *IN25         IFEQ      '1'
     C                   MOVE      *BLANK        SCSBNM                         ADD 11/08/93
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL     SMNAME        SCSBNM
     C                   END
     C*** CASE VALIDATION
     C                   Z-ADD     SCFND         XXFND             3 0
     C                   Z-ADD     SCFYR         XXFYR             3 0
     C                   Z-ADD     SCCASE        XXCASE            7 0
     C     CMKEY         CHAIN     PCM01                              21        INVALID CASE
     C     *IN21         CABEQ     '1'           STRT
     C*
     C*** FROM DATE VALIDATION
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     22    INVALID FRDT
     C*
     C*** TO DATE VALIDATION
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         CABEQ     '1'           STRT                     23    INVALID TODT
     C*
     C*** VALIDATE THE FROM DATE IS LE TODATE
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C     XXTODT        CABLT     XXFRDT        STRT                   24      TODT B4 FRDT
     C*
     C*** LOAD SCR02 DATA
     C*
     C* * * *  SS#/ ACCIDENT DATE
     C*
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      CMSS#         SCSS#
      /Free
       scSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C* * * *  LAST,FIRST NAME SUBPROGRAM FOR CLAIMANT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANK        SRLNAM           56
     C                   MOVEL     CDLSN         SRLNAM
     C                   MOVEL     CDFSN         SRFNAM           15
     C                   MOVEL     ' '           SRTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SRTERM
     C                   MOVEL     SRLNAM        SCCLNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       SCCLNM
     C                   END                                                    ENDIF *IN60
     C***       SUBROUTINE TO GET AND LOAD THE ACCOUNTS RECEIVABLE DETAIL
     C***       SUBFILE.
     C*
     C     INIT          IFNE      '1'                                          SETLL ONLY 1ST
     C     ARKEY         CHAIN     PAR01                              53
     C*** BEG CHG *** 01/07/99 ***************************************
     C                   MOVE      ARINV#        HLINV#            9 0
     C**********           MOVE ARINV#    HLINV#  50
     C*** END CHG *** 01/07/99 ***************************************
     C                   Z-ADD     0             REC1              4 0
     C                   Z-ADD     0             REC2              4 0
     C                   Z-ADD     0             REC2HL            4 0
     C                   Z-ADD     0             SCPBAL
     C                   Z-ADD     0             SCIAMT
     C                   Z-ADD     0             SCPAMT
     C                   Z-ADD     0             SCNAMT
     C                   Z-ADD     0             SCVAMT
     C                   Z-ADD     0             SCAAMT
     C                   Z-ADD     0             SCADUE
     C                   EXSR      ARLOAD
     C     ARKEY1        SETLL     PAR01
     C     ARKEY         READE     PAR01                                  53
     C                   EXSR      ARDETL
     C                   MOVE      '1'           INIT              1
     C                   END
     C*
     C                   MOVE      *BLANKS       Q2RECF                         GOTO DISPLAY
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*************************************
     C*   PROCESS REQUEST SCREEN - SCR02  *
     C*************************************
     C     SCR02T        TAG
     C*
     C                   MOVE      *BLANKS       Q2RECF                         GOTO DISPLAY
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
      *==================================================================*
      * ACCOUNTS RECEIVABLE LOAD TO SFL **SUBROUTINE**       A R L O A D *
      * SUMMARY SECTIONS ONLY                                            *
      *==================================================================*
     C*
     C     ARLOAD        BEGSR
     C                   Z-ADD     0             X                 5 0
     C*
     C     *IN53         DOWEQ     '0'
     C     ARTRDT        ANDLE     XXTODT
     C     X             ANDLT     99999
     C*
     C*** CREATE HEADING A/R INFO ***
     C*
     C*  A/R TRANSACTION DATE COMPARED TO SCREEN FROM DATE
     C*
     C     ARTRDT        IFLT      XXFRDT
     C     ARTRTP        IFEQ      'P'
     C                   SUB       ARAMT         SCPBAL
     C                   ELSE
     C                   ADD       ARAMT         SCPBAL
     C                   END
     C                   END
     C*
     C*  A/R TRANSACTION DATE WITHIN DATE RANGE
     C*
     C     ARTRDT        IFGE      XXFRDT
     C     ARTRDT        ANDLE     XXTODT
     C*
     C*** HOLD THE FIRST INVOICE THAT FALLS WITHIN THE RANGE TO USE
     C*** IN SETLL/READE IN ARDETL SUBROUTINE
     C     HOLD          IFEQ      'N'
     C*** BEG CHG *** 01/07/99 ***************************************
     C                   Z-ADD     ARINV#        XXINV#            9 0
     C**********           Z-ADDARINV#    XXINV#  50
     C*** END CHG *** 01/07/99 ***************************************
     C                   MOVE      'Y'           HOLD              1
     C                   END
     C*
     C     ARTRTP        IFEQ      'P'
     C                   ADD       ARAMT         SCPAMT
     C                   END
     C*
     C     ARTRTP        IFEQ      'A'
     C                   ADD       ARAMT         SCAAMT
     C                   END
     C*
     C     ARTRTP        IFEQ      'I'
     C                   ADD       ARAMT         SCIAMT
     C                   END
     C*
     C     ARTRTP        IFEQ      'V'
     C                   ADD       ARAMT         SCVAMT
     C                   END
     C*
     C     ARTRTP        IFEQ      'S'
     C                   ADD       ARAMT         SCSAMT
     C                   END
     C*
     C     ARTRTP        IFEQ      'N'
     C                   ADD       ARAMT         SCNAMT
     C                   END
     C*
     C                   END                                                    END GE/LE
     C*
     C*  ADD TO INVOICE SUMMARY BUCKETS REGARDLESS OF DATE
     C*
     C     ARTRTP        IFEQ      'P'
     C                   ADD       ARAMT         SCPAAM
     C                   END
     C*
     C     ARTRTP        IFEQ      'A'
     C                   ADD       ARAMT         SCAJAM
     C                   END
     C*
     C     ARTRTP        IFEQ      'I'
     C                   ADD       ARAMT         SCINAM
     C     ARTRDT        IFLT      HLINDT
     C     HLINDT        OREQ      *ZERO
     C                   Z-ADD     ARTRDT        HLINDT
     C                   END
     C                   END
     C*
     C                   ADD       1             X
     C*
     C     ARKEY         READE     PAR01                                  53
     C*
     C*  IF EOF OR INV# CHANGES, WRITE INVOICE SUMMARY RECORD (SCR02S1)
     C*
     C     *IN53         IFEQ      '1'
     C     ARINV#        ORNE      HLINV#
     C                   Z-ADD     HLINV#        SCINV#
     C*
     C                   Z-ADD     HLINDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCINDT
     C*
     C     SCINAM        SUB       SCPAAM        SCTDUE
     C                   ADD       SCAJAM        SCTDUE
     C     HLINDT        IFLT      XXFRDT
     C                   ADD       1             REC2HL
     C                   END
     C                   ADD       1             REC1
     C                   WRITE     SCR02S1
     C                   Z-ADD     0             SCINAM
     C                   Z-ADD     0             SCPAAM
     C                   Z-ADD     0             SCAJAM
     C                   Z-ADD     0             SCTDUE
     C                   Z-ADD     0             SCINDT
     C                   Z-ADD     0             HLINDT
     C                   Z-ADD     ARINV#        HLINV#
     C*                                                                **
     C*  RESET COUNTER TO ELIMINATE AN EXTRA RECORD ON SUMMARY LINE
     C                   Z-ADD     0             X
     C                   END                                                    END EQ *IN53
     C*
     C                   END                                                    END DOWEQ 53
     C*
     C*** CALCULATE CURRENT ACTIVITY DUE
     C*
     C     SCPBAL        SUB       SCPAMT        SCADUE
     C                   ADD       SCAAMT        SCADUE
     C                   ADD       SCIAMT        SCADUE
     C                   ADD       SCVAMT        SCADUE
     C                   ADD       SCSAMT        SCADUE
     C                   ADD       SCNAMT        SCADUE
     C*
     C*** IF AT LEAST ONE RECORD WAS READ THEN DISPLAY THE SUMMARY LINE
     C*
     C     X             IFGT      0
     C*
     C                   Z-ADD     HLINDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCINDT
     C*
     C                   Z-ADD     HLINV#        SCINV#
     C     SCINAM        SUB       SCPAAM        SCTDUE
     C                   ADD       SCAJAM        SCTDUE
     C                   ADD       1             REC1
     C                   WRITE     SCR02S1
     C                   Z-ADD     0             SCINAM
     C                   Z-ADD     0             SCPAAM
     C                   Z-ADD     0             SCAJAM
     C                   Z-ADD     0             SCTDUE
     C                   Z-ADD     0             SCINDT
     C                   Z-ADD     0             HLINDT
     C                   Z-ADD     ARINV#        HLINV#
     C                   END                                                    END X GT 0
     C*
     C*** CHECK IF NO RECORDS HAVE BEEN WRITTEN TO SFL, WRITE ONE BLANK
     C*
     C     REC1          IFLT      1
     C                   MOVE      *ZERO         SCINDT
     C                   MOVE      *ZERO         SCINV#
     C                   MOVE      *ZERO         SCINAM
     C                   MOVE      *BLANK        SCPAAM
     C                   MOVE      *BLANK        SCAJAM
     C                   MOVE      *BLANK        SCTDUE
     C                   ADD       1             REC1
     C                   Z-ADD     0             REC2HL
     C                   WRITE     SCR02S1
     C                   END
     C*
     C                   ADD       1             REC2HL
     C*
     C                   Z-ADD     1             SRCD#1
     C*
     C                   ENDSR
      *==================================================================*
      * ACCOUNTS RECEIVABLE LOAD TO SFL **SUBROUTINE**       A R D E T L *
      *==================================================================*
     C*
     C     ARDETL        BEGSR
     C                   Z-ADD     0             X                 5 0
     C*
     C     *IN53         DOWEQ     '0'
     C     X             ANDLT     4
     C*
     C     ARTRDT        IFGE      XXFRDT
     C     ARTRDT        ANDLE     XXTODT
     C                   Z-ADD     ARINV#        SCIN#3
     C                   Z-ADD     AREOB#        SCEOB3
     C                   MOVE      ARSYS         SCSYS
     C*
     C                   Z-ADD     ARTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTDT3
     C*
     C                   Z-ADD     ARPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPDT3
     C*
     C                   MOVEL     ARTRCD        SCTCD3
     C                   MOVEL     ARTRTP        SCTRTP
     C                   MOVEL     ARDESC        SCDSC3
     C                   Z-ADD     ARAMT         SCAMT3
     C                   Z-ADD     ARCK#         SCCHK3
     C*
     C                   ADD       1             X
     C                   ADD       1             REC2
     C                   WRITE     SCR02S2
     C                   END                                                    END GE/LE
     C*
     C     ARKEY         READE     PAR01                                  53
     C                   END                                                    END DOWEQ 53
     C*
     C*  CONDITION THE ROLLUP FUNCTION
     C*
     C     *IN53         IFEQ      '0'
     C     ARTRDT        IFLE      XXTODT
     C                   MOVE      '1'           *IN93
     C                   ELSE                                                   ELSE 0 *IN53
     C                   MOVE      '0'           *IN93
     C                   END                                                    END 0 *IN53
     C*
     C                   ELSE                                                   ELSE 0 *IN53
     C                   MOVE      '0'           *IN93
     C                   END                                                    END 0 *IN53
     C*
     C     REC2          IFLT      1
     C                   MOVE      *ZERO         SCIN#3
     C                   MOVE      *ZERO         SCEOB3
     C                   MOVE      *BLANK        SCSYS
     C                   MOVE      *ZERO         SCTDT3
     C                   MOVE      *ZERO         SCPDT3
     C                   MOVE      *BLANK        SCTCD3
     C                   MOVE      *BLANK        SCTRTP
     C                   MOVE      *BLANK        SCDSC3
     C                   MOVE      *ZERO         SCAMT3
     C                   MOVE      *ZERO         SCCHK3
     C                   ADD       1             REC2
     C                   WRITE     SCR02S2
     C                   END
     C*
     C                   Z-ADD     REC2          SRCD#
     C*
     C                   ENDSR
