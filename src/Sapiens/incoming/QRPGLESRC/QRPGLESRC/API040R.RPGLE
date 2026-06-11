     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE API040R - A/P CHECK REQUEST MAINTENANCE
     H******************************************************************
     H*  DESCRIPTION:
     H*
     H*    SCR01A added to obtain Company # and Facility #.
     H*
     H*    Using CO# and Bank#, access current Bank Account (APBIP).
     H*    Using the Bank Account #, access WCMS Bank Account Master
     H*    (WSCKP) to obtain "Starting Check #".
     H*
     H*    Pass Company#, Bank Account#, Check Formtype, Check OUTQ,
     H*    and Hard copy flag as PARMS to pgm APB041C.
     H*
     H*  CALLED FROM: MENU  (A/P, PROCESS VOUCHER DATA)
     H*  CALLS TO   : APB041C
     H*
     H******************************************************************
     H*  REVISIONS:
     H*
     H*    12/08/97 C:30 Project: WC500
     H*             Implement above description changes for V3.
     H******************************************************************
     FAPI040F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FAPBKL1    UF   E           K DISK
     FAPCSL2    IF   E           K DISK
     FBKCRL2    IF   E           K DISK
     FBKCFL1    IF   E           K DISK
     FBKDTL1    IF   E           K DISK
      *** BEGIN ADD *** 12/08/97 ***************************************
      *
      *  Bank Account Master
     FWSCKL1    IF   E           K DISK
      *** END   ADD *** 12/08/97 ***************************************

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C*   KEY LIST FOR THE COMPANY/FACILITY MASTER FILE.
     C*
     C     CFKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C*
     C*
     C*   KEY LIST FOR THE CHECK REC FILE
     C*
     C     CRKEY         KLIST
     C                   KFLD                    SCAPP             2
     C                   KFLD                    SCBNK#
     C                   KFLD                    SCCHK#
     C*
     C*
     C*   KEY LIST FOR THE CHECK SELECTION FILE
     C*
     C     CSKEY         KLIST
     C                   KFLD                    SCBNK#
     C                   KFLD                    XKCHK#            7 0
      *
      *** BEGIN ADD *** 12/08/97 *******************************************
      *
      *   WCMS Bank Account Master file
      *
     C     CKKEY         KLIST
     C                   KFLD                    ##ACCT
      *
      *** END   ADD *** 12/08/97 *******************************************
     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
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
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
      *** BEGIN CHG *** 12/08/97 ***************************************
     C*****                MOVEL'SCR01'   Q2RECF
     C*****                Z-ADD001       SCCO#   30
     C*****                Z-ADD001       SCFAC#  30
      *
     C                   MOVEL     'SCR01A'      Q2RECF
      *
      *** END   CHG *** 12/08/97 ***************************************
      *** BEGIN DEL *** 12/08/97 ***************************************
     C*****
     C***GET THE REQUESTED COMPANY/FACILITY RECORD.
     C*****
     C*****      CFKEY     CHAINPCF01                21
     C*****      *IN21     IFEQ '0'
     C*****                Z-ADDCFBNK#    SCBNK#
     C*****                Z-ADDCFBNK#    BFBNK#  30
     C*****      SCBNK#    CHAINPBK01                60
     C**N60                EXCPTPBK01R
     C**N60      BKLCHK    ADD  1         SCCHK#
     C*****                END
     C*****
     C*****
     C***GET THE REQUESTED RECORD, LOAD INFORMATION
     C*****
     C*****      'AP'      CHAINPDT01                19
     C*****      *IN19     IFEQ '0'
     C*****
     C*****                Z-ADDDTAPDT    SRDTI
     C*****                CALL 'SRCVTDAT'DTMDYY
     C*****                MOVELSRDTO     SCDATE
     C*****
     C*****                END
      *** END   DEL *** 12/08/97 ***************************************
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
      *** BEGIN DEL *** 12/08/97 ***********************************************
     C*****                MOVELQ2FILE    SCFILE           SET FORMAT/FILE
     C*****                MOVELQ2RECF    SCRECF            FOR DISPLAY
      *** END   DEL *** 12/08/97 ***********************************************
     C                   MOVEL     Q1USER        SCUSER
      *
      *** BEGIN ADD *** 12/08/97 ***************************************
      *
     C     Q2RECF        IFEQ      'SCR01A'
     C                   EXFMT     SCR01A
     C                   END
      *
      *** END   ADD *** 12/08/97 ***************************************
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C********************************************
     C*   F2 = RESTART CHECK PRINTING            *
     C********************************************
     C     *INKB         IFEQ      '1'
     C                   MOVE      'Y'           SCVOID
     C                   MOVEL     'SCR02 '      Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *INKB
     C********************************************
     C*   F3 = EXIT                              *
     C********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C********************************************
     C*   F10 = SEARCH                           *
     C********************************************
      *** BEGIN CHG *** 12/08/97 ***********************************************
     C************INKE     IFEQ '1'
      *
     C     *INKJ         IFEQ      *ON
      *** END   CHG *** 12/08/97 ***********************************************
      *** BEGIN ADD *** 12/08/97 ***********************************************
     C     Q2RECF        IFEQ      'SCR01A'
     C                   MOVE      '1'           QQRTC
     C                   CALL      'BKD001R'
     C                   PARM                    SCCO#
     C                   PARM                    SCFAC#
     C                   PARM                    QQRTC             1
     C     QQRTC         IFEQ      'A'
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01A'      Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  END SCR01A
      *** END   ADD *** 12/08/97 ***********************************************
     C*********
     C* SCR01 *
     C*********
      *
      *** BEGIN CHG *** 12/08/97 ***********************************************
     C*****                MOVE '1'       QQRTCD
     C*****      Q2RECF    IFEQ 'SCR01'
     C*****                MOVE *ZEROS    XKBNK#  30
     C*****                CALL 'BKD020R'
     C*****                PARM           XKBNK#
     C*****                PARM           QQRTCD  1
     C*****      QQRTCD    CABEQ'A'       TERM
     C*****      XKBNK#    IFGT *ZERO
     C*****                MOVE XKBNK#    SCBNK#
      *
     C     Q2RECF        IFEQ      'SCR01'
      *
     C                   MOVE      SCCO#         ##FND                          AP Company #
      *
     C                   CALL      'A2420R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##ACCT           20
     C                   PARM                    QQEFDT            8
     C                   PARM                    ##APBK            3
      *
     C     *INKC         CABEQ     *ON           TERM                     LR
      *
     C*
     C     ##APBK        IFGT      *ZERO
     C                   MOVE      ##APBK        SCBNK#
      *
      *** END   CHG *** 12/08/97 ***********************************************
      *** BEGIN CHG *** 12/08/97 ***********************************************
      *
      *  Edit Bank #, Bank Information, and Bank Account #
      *
     C                   EXSR      SRBANK
      *
     C     RCBANK        IFNE      *BLANK
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  SCBNK# NE BFBNK#
      *
     C     SCBNK#        IFNE      BFBNK#
     C                   MOVE      SCBNK#        BFBNK#
     C                   Z-ADD     CKLCHK        SCCHK#
     C                   ENDIF                                                  SCBNK# NE BFBNK#
      *
     C******               MOVE SCBNK#    BFBNK#
     C******     SCBNK#    CHAINPBK01                60
     C**N60*     BKLCHK    ADD  1         SCCHK#
      *** END   CHG *** 12/08/97 ***********************************************
      *
     C                   END                                                    XKBNK# > 0
     C                   WRITE     SCR99
     C                   MOVE      '1'           *IN34
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*********
     C* SCR02 *
     C*********
     C                   MOVE      *ZEROS        XKBNK#            3 0
     C                   CALL      'BKD020R'
     C                   PARM                    XKBNK#
     C                   PARM                    QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     XKBNK#        IFGT      *ZERO
     C                   MOVE      XKBNK#        SCBNK#
      *
      *** BEGIN CHG *** 12/08/97 ***********************************************
      *
      *  Edit Bank #, Bank Information, and Bank Account #
      *
     C                   EXSR      SRBANK
      *
     C     RCBANK        IFNE      *BLANK
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  SCBNK# NE BFBNK#
      *
     C     SCBNK#        IFNE      BFBNK#
     C                   MOVE      SCBNK#        BFBNK#
     C                   Z-ADD     CKLCHK        SCCHK#
     C                   Z-ADD     CKLCHK        SCCHK2
     C                   ENDIF                                                  SCBNK# NE BFBNK#
      *
     C*****                MOVE SCBNK#    BFBNK#
     C*****      SCBNK#    CHAINPBK01               N60
      *****
     C*****      SCBNK#    CHAINPBK01                60
     C**N60                EXCPTPBK01R
     C**N60      BKLCHK    ADD  1         SCCHK2
      *** END   CHG *** 12/08/97 ***********************************************
      *
     C                   END                                                    XKBNK# > 0
     C                   WRITE     SCR99
     C                   MOVE      '1'           *IN34
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END                                                    END SCR02
     C                   END                                                    END *INKF
     C********************************************
     C*   F9 = COMPANY FACILITY INQUIRY          *
     C********************************************
     C     *INKI         IFEQ      '1'
     C*********
     C* SCR01 *
     C*********
     C                   MOVE      '1'           QQRTCD
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'BKD001R'
     C                   PARM                    SCCO#
     C                   PARM                    SCFAC#
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      '1'           *IN34
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END                                                    END SCR02
     C                   END                                                    END *INKF
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END                                                    END SCR02
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C**
     C                   END                                                    END CMDKEYS
     C**                                                               ********
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
      *** BEGIN ADD *** 12/08/97 ***************************************
     C     Q2RECF        CABEQ     'SCR01A'      SCR01X
      *** END   ADD *** 12/08/97 ***************************************
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
      *** BEGIN ADD *** 12/08/97 ***************************************
      *
     C     SCR01X        TAG
      *
      *  AP/GL CO# must be greater than zero
      *
     C     SCCO#         IFLE      *ZERO
     C                   MOVE      *ON           *IN20
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  AP/GL FAC# must be greater than zero
      *
     C     SCFAC#        IFLE      *ZERO
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  Access the Requested Company/Facility record.
      *
     C     CFKEY         CHAIN     PCF01                              22
      *
     C     *IN22         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  Access the System Date file and Assign AP Date
      *
     C     'AP'          CHAIN     PDT01                              19
     C     *IN19         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     *IN19         IFEQ      '0'
      *
     C                   Z-ADD     DTAPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDATE
      *
     C                   END
      *
     C                   Z-ADD     CFBNK#        SCBNK#
     C                   WRITE     SCR01
      *
      *  Edit Bank #, Bank Information, and Bank Account #
      *
     C                   EXSR      SRBANK
      *
      *  If an error is detected, the error msg will display on SCR01
      *  Go to the Next Screen
      *
     C     RCBANK        IFEQ      *BLANK
     C                   Z-ADD     CFBNK#        BFBNK#            3 0
     C                   ENDIF
      *
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
      *** END   ADD *** 12/08/97 ***************************************
      *
     C     SCR01T        TAG
     C*
     C*  THE CHECK DATE MUST BE VALID.
     C*
     C     SCDATE        IFEQ      *ZERO
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ELSE
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   END
      *
      *** BEGIN ADD *** 12/08/97 ***********************************************
      *
      *  Edit Bank #, Bank Information, and Bank Account #
      *
     C                   EXSR      SRBANK
      *
      *  If an error is detected, the error msg will display on SCR01
      *
     C     RCBANK        IFNE      *BLANK
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  RCBANK NE Blank
      *
     C     SCBNK#        IFNE      BFBNK#
     C                   MOVE      SCBNK#        BFBNK#
     C                   Z-ADD     CKLCHK        SCCHK#
     C                   ENDIF                                                  SCBNK# NE BFBNK#
      *
      *** END   ADD *** 12/08/97 ***********************************************
     C*
     C*  MAKE SURE THERE ARE CHECKS TO PRINT
     C*
     C     SCBNK#        CHAIN     PCS01                              23
     C     *IN23         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   Z-ADD     1             XKCHK#
     C     CSKEY         SETLL     PCS01
     C     SCBNK#        READE     PCS01                                  60
     C  N60CSCHK#        IFGT      *ZERO
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK CANNOT ALREADY BE IN CHECK REC FILE
     C*
     C                   MOVE      'AP'          SCAPP
     C     CRKEY         CHAIN     PCR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED BANK RECORD.
     C*
     C     SCBNK#        CHAIN     PBK01                              22
     C     *IN22         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
      *
      *** BEGIN DEL *** 12/08/97 ***********************************************
     C*****      SCBNK#    IFNE BFBNK#
     C*****      BKLCHK    ADD  1         SCCHK#
     C*****                MOVE SCBNK#    BFBNK#
     C*****                GOTO STRT
     C*****                END
     C*****
     C***START CHECK PRINTING
     C*****
     C*****      SCCHK#    SUB  1         BKLCHK
     C*****                ADD  SCLEAD    BKLCHK
      *** END   DEL *** 12/08/97 ***********************************************
      *** BEGIN ADD *** 12/08/97 ***********************************************
      *
      *  Syncronize AP Bank record Check # with WCMS Bank Account Check #
      *
     C                   Z-ADD     CKLCHK        BKLCHK
      *
      *** END   ADD *** 12/08/97 ***********************************************
      *
     C                   UPDATE    PBK01
     C*
     C                   MOVE      *BLANKS       QQKEY            40
     C                   MOVE      *BLANKS       QQQUE            10
     C                   MOVE      *BLANKS       QQRTCD
     C                   MOVE      *ZEROS        SCCHK1
     C                   MOVE      *BLANKS       SCVOID
      *** BEGIN ADD *** 12/08/97 ***********************************************
     C                   MOVE      SCDATE        QQDATE            8
      *** END   ADD *** 12/08/97 ***********************************************
      *
     C                   CALL      'APB041C'                                                B
     C                   PARM                    QQKEY
     C                   PARM                    QQQUE
     C                   PARM                    QQRTCD            1
     C                   PARM                    SCBNK#
      *** BEGIN CHG *** 12/08/97 ***********************************************
     C*********************PARM           SCDATE
     C                   PARM                    QQDATE
      *** BEGIN CHG *** 12/08/97 ***********************************************
     C                   PARM                    SCCHK1
     C                   PARM                    SCVOID
      *** BEGIN ADD *** 12/08/97 ***********************************************
     C                   PARM                    ##FND             3            AP/GL CO #
     C                   PARM                    ##ACCT           20            Bank Acct #
     C                   PARM                    ##FTYP           10            Form Type
     C                   PARM                    ##CHKQ           10            Check OUTQ
     C                   PARM                    ##HDCY            1            Hardcopy Y/N
      *** END   ADD *** 12/08/97 ***********************************************
     C*
     C                   GOTO      TERM
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR02T        TAG
     C*
     C*  THE CHECK DATE MUST BE VALID.
     C*
     C     SCDATE        IFEQ      *ZERO
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ELSE
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   END
      *
      *** BEGIN ADD *** 12/08/97 ***********************************************
      *
      *  Edit Bank #, Bank Information, and Bank Account #
      *
     C                   EXSR      SRBANK
      *
      *  If an error is detected, the error msg will display on SCR01
      *
     C     RCBANK        IFNE      *BLANK
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  RCBANK NE Blank
      *
     C     SCBNK#        IFNE      BFBNK#
     C                   MOVE      SCBNK#        BFBNK#
     C                   Z-ADD     CKLCHK        SCCHK#
     C                   Z-ADD     CKLCHK        SCCHK2
     C                   ENDIF                                                  SCBNK# NE BFBNK#
      *
      *** END   ADD *** 12/08/97 ***********************************************
     C*
     C*  MAKE SURE THERE ARE CHECKS TO PRINT
     C*
     C     SCBNK#        CHAIN     PCS01                              23
     C     *IN23         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED BANK RECORD.
     C*
     C     SCBNK#        CHAIN     PBK01                              22
     C     *IN22         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
      *
      *** BEGIN DEL *** 12/08/97 ***********************************************
     C*****      SCBNK#    IFNE BFBNK#
     C*****      BKLCHK    ADD  1         SCCHK2
     C*****                MOVE SCBNK#    BFBNK#
     C*****                GOTO STRT
     C*****                END
     C*****
     C***CALL PROGRAM TO REPRINT CHECKS
     C*****      SCCHK2    SUB  1         BKLCHK
     C*****                ADD  SCLEAD    BKLCHK
      *** END   DEL *** 12/08/97 ***********************************************
      *** BEGIN ADD *** 12/08/97 ***********************************************
      *
     C                   Z-ADD     CKLCHK        BKLCHK
      *
      *** END   ADD *** 12/08/97 ***********************************************
      *
     C                   UPDATE    PBK01
     C*
     C                   MOVE      *BLANKS       QQKEY            40
     C                   MOVE      *BLANKS       QQQUE            10
     C                   MOVE      *BLANKS       QQRTCD
      *** BEGIN ADD *** 12/08/97 ***********************************************
     C                   MOVE      SCDATE        QQDATE            8
      *** END   ADD *** 12/08/97 ***********************************************
      *
     C                   CALL      'APB041C'                                                B
     C                   PARM                    QQKEY
     C                   PARM                    QQQUE
     C                   PARM                    QQRTCD
     C                   PARM                    SCBNK#
      *** BEGIN CHG *** 12/08/97 ***********************************************
     C*********************PARM           SCDATE
     C                   PARM                    QQDATE
      *** BEGIN CHG *** 12/08/97 ***********************************************
     C                   PARM                    SCCHK1
     C                   PARM                    SCVOID
      *** BEGIN ADD *** 12/08/97 ***********************************************
     C                   PARM                    ##FND             3            AP/GL CO #
     C                   PARM                    ##ACCT           20            Bank Acct #
     C                   PARM                    ##FTYP           10            Form Type
     C                   PARM                    ##CHKQ           10            Check/Voucher OUTQ
     C                   PARM                    ##HDCY            1            Hardcopy Y/N
      *** END   ADD *** 12/08/97 ***********************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
      *** BEGIN ADD *** 12/08/97 ***********************************************
      **************************************************************************
      *  SRBANK - Edit:  Is Bank# entered valid?
      *           3 Files are accessed to determine answer.
      *           APBKP is accessed to verify Bank# exists.
      *           APBIP is accessed using CO# & Bank# to verify Bank Account
      *           exists as well as the Bank Account Number.
      *           WSCKP (WCMS Bank Account Master) is accessed by Account#
      *           to obtain "Starting Check #".
      *           Return var RCBANK with outcome:
      *              ' ' = Bank# is valid (all of the above occurred)
      *              '1' = APBKP rec not found
      *              '2' = APBIP rec not found for Bank#
      *              '3' = WSCKP rec not found
      **************************************************************************
     C     SRBANK        BEGSR
      *
      *  Access AP/GL Bank file (release update lock)
      *
     C                   MOVE      '1'           RCBANK            1            Default assign
      *
     C     SCBNK#        CHAIN(N)  PBK01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   EXSR      SRSCR
     C                   MOVE      SCBNK#        BFBNK#
     C                   MOVE      *ON           *IN31
     C                   ENDIF
      *
     C     *IN31         IFEQ      *OFF
      *
      *  Call AP/GL Bank Information API using DTAPDT as search date
      *
     C                   MOVE      '2'           RCBANK                         Default assign
     C                   MOVE      SCCO#         ##FND
     C                   MOVE      SCBNK#        ##APBK
     C                   MOVE      SCDATE        ##SRDT
      *
     C                   CALL      'AA023R'
     C                   PARM                    ##FND             3            AP/GL CO #
     C                   PARM                    ##APBK            3            AP/GL Bank #
     C                   PARM                    ##SRDT            8            MMDDCCYY
     C                   PARM      *BLANK        ##ACCT           20            Bank Acct #
     C                   PARM      'C'           ##CALL            1            (V)oucher/(C)heck
     C                   PARM      *BLANK        ##HDCY            1            Hardcopy Y/N
     C                   PARM      *BLANK        ##CHKQ           10            Check/Voucher OUTQ
     C                   PARM      *BLANK        ##FTYP           10            Form Type
     C                   PARM      *BLANK        ##RTCD            1            Return Code
     C                   PARM      *ZERO         ##EFDT            7            APBIP Eff. DT CYYMMD
      *
     C     ##RTCD        IFNE      *BLANK
     C                   Z-ADD     *ZERO         SCCHK#
     C                   EXSR      SRSCR
     C                   MOVE      SCBNK#        BFBNK#
     C                   MOVE      *ON           *IN32
     C                   ENDIF
      *
     C     *IN32         IFEQ      *OFF
     C                   MOVE      '3'           RCBANK                         Default assign
      *
      *  Access WCMS Bank Account Master
      *
     C     CKKEY         CHAIN     PCK01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     *ZERO         SCCHK#
     C                   EXSR      SRSCR
     C                   MOVE      SCBNK#        BFBNK#
     C                   MOVE      *ON           *IN33
     C                   ENDIF
      *
     C     *IN33         IFEQ      *OFF
     C                   MOVE      *BLANK        RCBANK                         Passed ALL Edits
     C     SCBNK#        IFNE      BFBNK#
     C                   Z-ADD     CKLCHK        SCCHK#
     C                   Z-ADD     CKLCHK        SCCHK2
     C                   ENDIF                                                  SCBNK# NE BFBNK#
     C                   EXSR      SRSCR
     C                   ENDIF                                                  *IN33
     C                   ENDIF                                                  *IN32
     C                   ENDIF                                                  *IN31
      *
     C                   ENDSR
      *
      **************************************************************************
      *  SRSCR - Write proper screen prior to error message
      **************************************************************************
     C     SRSCR         BEGSR
      *
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR01A'
     C                   WRITE     SCR01A
     C     Q2RECF        WHENEQ    'SCR01'
     C                   WRITE     SCR01
     C     Q2RECF        WHENEQ    'SCR02'
     C                   WRITE     SCR02
     C                   ENDSL
      *
     C                   ENDSR
      *** END   ADD *** 12/08/97 ***********************************************
      *
      *** BEGIN DEL *** 12/08/97 ***********************************************
     O*****PBK01   E                PBK01R
      *** END   DEL *** 12/08/97 ***********************************************
