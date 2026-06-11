    HCOPYRIGHT('(C) Copyright Fiserv, Inc., 2008')
    H*********************************************************************
    H/TITLE EDI Auto Trigger Process
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DEBUG(*YES)
    H*********************************************************************
    H*  CALLED FROM: MENU                                                *
    H*                                                                   *
    H*  CALLS TO: W3017C                                                 *
    H*                                                                   *
    H*  CREATED: 07/25/08      C:183     PROJECT: 114WQS                 *
    H*********************************************************************
    H*  REVISIONS                                                        *
    H*                                                                   *
    H*********************************************************************
      *
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW3017F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      *
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWXGSL     IF   E           K DISK
     FWMACL12   IF   E           K DISK
      *
     D OPTNM           C                   CONST('Agent/Rate changes since l-
     D                                     ast commission run - Prompt')
      *
     D @ACTRDT         S               D   DATFMT(*USA)
     D @CURDT          S               D   DATFMT(*USA)
     D @@FOUND         S              1
     D @MTHDIF         S              2  0
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C*_______________________________________________________________
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
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C*
     C     ACKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    GSFND
     C*
     C     *ENTRY        PLIST
     C                   PARM      *IN03         @@KC              1
     C                   PARM      *IN12         @@KL              1
     C*_______________________________________________________________
     C*
     C                   TIME                    DSDTTM
     C*_______________________________________________________________
     C*
     C     TOP           TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     Q1USER        CHAIN     WMUPL                              60
     C*
     C   60              MOVE      *ZEROS        UPCO#
     C*
     C*  GET COMPANY NAME
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C   60              MOVEL     *BLANKS       CONAME
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C*_______________________________________________________________
     C*
     C     *LOVAL        SETLL     WXGSL
     C                   READ      WXGSL
      *
     C                   DOW       NOT %EOF(WXGSL)
      *
     C                   IF        GSIEFL = 'I'
      *
     C     ACKY          SETGT     WMACL12
     C     ACKY          READPE    WMACL12
      *
     C                   DOW       NOT %EOF(WMACL12)
      *
     C                   IF        ACCOTP = 'P'
      *
     C                   EVAL      SRDTI = ACTRDT
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   IF        SRERR <> '1'
     C                   EVAL      @ACTRDT = %DATE(SRDTO:*USA0)
     C                   ENDIF
      *
     C*PUIMOD
     C*                  MOVE      *DATE         @CURDT
     C                   EVAL      @CURDT = %Date()
      *
     C                   EVAL      @MTHDIF = %DIFF(@CURDT:@ACTRDT:*MONTHS)
      *
     C                   IF        @MTHDIF >= 1
      *
     C                   EXFMT     WDW01
     C*
     C                   SELECT
     C*
     C                   WHEN      *IN12 = *ON
     C
     C                   EVAL      @@KL = '1'
     C*
     C                   ENDSL
     C*
     C*  F12=Cancel
     C*
     C     *IN12         CABEQ     *ON           TERM                     LR
      *
     C                   LEAVE
      *
     C                   ENDIF                                                  @MTHDIF >= 1
      *
     C                   ENDIF                                                  ACCOTP = 'P'
      *
     C     ACKY          READPE    WMACL12
      *
     C                   ENDDO                                                  NOT %EOF(WMACL12)
      *
     C                   ENDIF                                                  GSIEFL = 'I'
      *
     C                   READ      WXGSL
      *
     C                   ENDDO                                                  NOT %EOF(WXGSP)
      *
     C*_______________________________________________________________
     C*
     C*  Call the CLP to Submit the Program
     C*
      *
     C                   MOVE      UPCO#         QQCO#             3
      *
     C                   CALL      'W3017C'
     C                   PARM                    QQCO#
      *
     C                   EVAL      *IN03 = *ON
     C*_______________________________________________________________
     C*
     C*  End the Program
     C*
     C     TERM          TAG
     C*
     C                   MOVE      *ON           *INLR
