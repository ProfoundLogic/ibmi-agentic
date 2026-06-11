     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W4220R - Calculate New Balance & Update Check Summary File
     H*****************************************************************
     H*
     H* REVISIONS:
     H*
     H* 01/24/01 C:61  SUPPORT WA099  25700
     H*              - Return if F3 or F12 used to exit program.
     H*              - Add F12=Cancel to SCR03. -> SCR01
     H*
     H* 05/24/99 C:78 PROJECT: WC415
     H*               INQUIRY MODE SHOULD NOT ALLOW CMD17 ON SCR03.
     H*
     H* 12/07/95 C:22 PROJECT: SUPPORT
     H*               MODIFIED THE CALCULATION FOR BANK ERRORS.
     H*****************************************************************
     F*
     F* Prompt Screen
     FW4220F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter Master File
     FWMUPL     IF   E           K DISK
     F*
     F* Check Summary Master File
     FWMCSL     IF   E           K DISK
     F*
     F* Check Reconciliation Detail File
     FWDCRL4    IF   E           K DISK
     FWDCRL2    IF   E           K DISK
     F                                     RENAME(PCR01:PCR02)
     FWDCRL1    IF   E           K DISK
     F                                     RENAME(PCR01:PCR03)

     D*
     D* System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* Screen Reference Numbers
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D* External Program Data Structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* Key List for the Check Summary Master File
     C*
     C     CSKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXACCT
     C*
     C* Key List for the Check Reconciliation Detail File
     C*
     C     CRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXACCT
     C                   KFLD                    XXRDT1            7 0
     C*
     C     CRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXACCT
     C                   KFLD                    XXFRDT
     C*
     C     CRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXACCT
     C*
     C     CRKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXACCT
     C                   KFLD                    XXCHK#
     C*
     C     *LIKE         DEFINE    CRDT          XXFRDT
     C     *LIKE         DEFINE    CRDT          XXTODT
     C     *LIKE         DEFINE    CRRVDT        XXRVDT
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQACCT           20
      ***** BEG ADD ******* 01/24/01 **********************************
     C                   PARM      *INKC         QQKC              1
     C                   PARM      *INKL         QQKL              1
      ***** END ADD ******* 01/24/01 **********************************
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
     C* Load the Output Constants from the Passed Parms
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVEL     QQACCT        XXACCT           20
     C                   Z-ADD     0             SCDT
     C*
     C     QQCALL        IFEQ      '1'
     C                   MOVE      '1'           *IN09
     C                   END
     C*
     C* Get the User Parameter Record
     C*
     C     TOP           TAG
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C                   MOVEL     XXACCT        SCACCT
     C*
     C* Get the Company Record
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C* Load the Record Format Info for First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C* Display All Formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   WRITE     SCR02
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C*
     C* Process All Command Keys
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
      ***** BEG ADD ******* 01/24/01 **********************************
      *
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
      ***** END ADD ******* 01/24/01 **********************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      '1'           *INLR
     C                   END
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 05 = SEARCH LIST CALL
     C*
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      SCACCT        QQACCT
     C                   MOVE      SCDT          QQDATE
     C                   CALL      'W4235R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQACCT
     C                   PARM                    QQDATE            8
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQDATE        OREQ      *BLANKS
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      QQDATE        SCDT
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
     C*
     C*  COMMAND 17 - GENERATE REPORT AND UPDATE CHECK SUMMARY FILE
     C*
     C     *INKR         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVE      UPOUTQ        QQQUE            10
     C                   MOVE      SCACCT        QQACCT
     C                   MOVE      SCDT          QQTDT             8
     C                   CALL      'W4225C'
     C                   PARM                    QQCO#
     C                   PARM                    QQACCT
     C                   PARM                    QQTDT
     C                   PARM                    QQQUE
     C*
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C*
     C                   END
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
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
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* Goto the Appropriate Screen
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C*
     C* Process the Requested Screen
     C*
     C     SCR01T        TAG
     C*
     C* Ending date must be valid.
     C*
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C* THERE MUST NOT BE AN ACTIVE CHECK SIMMARY RECORD ON OR AFTER
     C* THE ENTERED ENDING DATE.
     C*
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C     CSKEY         SETGT     PCS01
     C                   READP     PCS01                                  60
     C     *IN60         DOWEQ     '0'
     C     CSSTAT        ANDEQ     'I'
     C                   READP     PCS01                                  60
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C     CSACCT        ANDEQ     XXACCT
     C     CSCO#         ANDEQ     XXCO#
     C*
     C* A summary record exists after the entered date
     C*
     C     CSEDAT        IFGE      XXTODT
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C* If date is valid, then load the start date and Old Balance.
     C*
     C                   Z-ADD     CSEDAT        XXFRDT
     C                   Z-ADD     CSEBAL        OLDBAL
     C                   ELSE
     C                   Z-ADD     0             XXFRDT
     C                   Z-ADD     0             OLDBAL            9 2
     C                   END
     C*
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFDT             8 0
     C*
     C* We have Passed Edits for Screen 1; Go To Next Screen.
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C* No User interaction on screen 2.
     C* Calculate New Balance.
     C*
     C* Initialize work fields.
     C*
     C                   Z-ADD     0             XXDEP             9 2          Deposits
     C                   Z-ADD     0             XXCHK             9 2          Checks
     C                   Z-ADD     0             XXTRN             9 2          Transfers
     C                   Z-ADD     0             XXNSF             9 2          NSF's
     C                   Z-ADD     0             XXADJ             9 2          Adjustments
     C                   Z-ADD     0             XXREC             9 2          Recoveries
     C                   Z-ADD     0             XXPSTP            9 2          Stop pymts
     C                   Z-ADD     0             XXCSTP            9 2          Stop pymts
     C                   Z-ADD     0             XXPREV            9 2          Reverses
     C                   Z-ADD     0             XXCREV            9 2          Reverses
     C                   Z-ADD     0             XXPVOD            9 2          Voids
     C                   Z-ADD     0             XXCVOD            9 2          Voids
     C                   Z-ADD     0             XXCHG             9 2          Bank charges
     C                   Z-ADD     0             XXERR             9 2          Bank errors
     C                   Z-ADD     0             XXINT             9 2          Interest
     C                   Z-ADD     0             NEWBAL            9 2           New balance
     C*
     C* CALCULATE NEW BALANCE BY GETTING ALL RECORDS WHICH HAVE BEEN
     C* UPDATED AFTER THE LAST ENDING BALANCE DATE & UP TO & INCLUDING
     C* THE NEW ENTERED ENDING DATE. INCLUDE ONLY THOSE RECORDS WHOSE
     C* 1ST RECONCILIATION DATE IS NOT 0.
     C*
     C* IF TRANS IS A REVERSAL, DETERMINE IF THE REVERSED TYPE HAS
     C* ALREADY BEEN INCLUDED AND IF NOT INCLUDE IN CALC.
     C*
     C     CRKY1         SETGT     PCR02                              60
     C     CRKY2         READE     PCR02                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     CRACDT        IFLE      XXTODT
     C     CRRDT1        ANDNE     0
     C     CRACTP        ANDNE     'X'
     C                   EXSR      SRCALC
     C                   END
     C*
     C     CRACDT        IFLE      XXTODT
     C     CRRTP         ANDEQ     'Z'
     C                   EXSR      SRCALC
     C     CRRVDT        IFGT      XXFRDT
     C*
     C                   Z-ADD     CRCHK#        XXCHK#            7 0
     C                   MOVE      CRRVTP        XXRVTP            1
     C                   Z-ADD     CRRVDT        XXRVDT
     C     CRKY3         CHAIN     PCR03                              60
     C     *IN60         DOWEQ     '0'                                          JJJ 10-2-89
     C     CRRTP         ANDNE     'C'                                          |
     C     CRKY3         READE     PCR03                                  60    |
     C                   END                                                    |
     C*                                                    |
     C     *IN60         IFEQ      '0'
     C     CRACTP        IFEQ      'V'
     C     CRACTP        OREQ      'S'
     C     CRACTP        OREQ      'X'
     C*
     C*  IF A TRANSACTION HAS BEEN ENTERED, STOPPED, REVERSED & VOIDED
     C*  IN THE SAME REC PERIOD, THEN WE NEED TO ACCOUNT FOR THE STOP
     C*  WHICH IS ONLY RECORDED ON THE REVERSE RECORD AT THIS POINT.
     C*  (THE STOP ON THE ORIGINAL CHECK HAS BEEN REPLACED BY A VOID)
     C*
     C     CRACTP        IFNE      XXRVTP
     C                   MOVE      'Y'           CRRVFL
     C                   MOVE      XXRVTP        CRACTP
     C                   Z-ADD     XXRVDT        CRACDT
     C                   EXSR      SRCALC
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C                   END
     C*
     C     CRKY2         READE     PCR02                                  60
     C                   END
     C*
     C* NOW INCLUDE ALL NEWLY ADDED RECORDS (CRRDT1 = 0), IF THE CHECK
     C* DATE IS LESS THAN OR EQUAL TO THE RECONCILIATION DATE ENTERED.
     C* WE DO NOT WANT TO RECONCILE TRANSACTIONS THAT OCCUR AFTER THIS
     C* ENTERED RECONCILIATION DATE.
     C*
     C                   Z-ADD     0             XXRDT1
     C     CRKEY         CHAIN     PCR01                              60
     C     *IN60         DOWEQ     '0'
     C     CRCKDT        IFLE      XXTODT
     C     CRRTP         ANDNE     'Z'
     C                   EXSR      SRCALC
     C                   END
     C     CRKEY         READE     PCR01                                  60
     C                   END
     C*
     C* Move work fields to screen fields
     C*
     C                   Z-ADD     OLDBAL        SCOBAL                         Old balance
     C                   Z-ADD     XXDEP         SCDEP             9 2          Deposits
     C                   Z-ADD     XXCHK         SCCHK             9 2          Checks
     C                   Z-ADD     XXTRN         SCTRN             9 2          Transfers
     C                   Z-ADD     XXNSF         SCNSF             9 2          NSF's
     C                   Z-ADD     XXADJ         SCADJ             9 2          Adjustments
     C                   Z-ADD     XXREC         SCREC             9 2          Recoveries
     C                   Z-ADD     XXPSTP        SCPSTP            9 2          Stop pymts
     C                   Z-ADD     XXCSTP        SCCSTP            9 2          Stop pymts
     C                   Z-ADD     XXPREV        SCPREV            9 2          Reverses
     C                   Z-ADD     XXCREV        SCCREV            9 2          Reverses
     C                   Z-ADD     XXPVOD        SCPVOD            9 2          Voids
     C                   Z-ADD     XXCVOD        SCCVOD            9 2          Voids
     C                   Z-ADD     XXCHG         SCCHG             9 2          Bank charges
     C                   Z-ADD     XXERR         SCERR             9 2          Bank errors
     C                   Z-ADD     XXINT         SCINT             9 2          Interest
     C*
     C* Calculate New Balance
     C*
     C                   Z-ADD     OLDBAL        NEWBAL
     C                   ADD       XXDEP         NEWBAL
     C                   SUB       XXCHK         NEWBAL
     C                   ADD       XXTRN         NEWBAL
     C                   SUB       XXNSF         NEWBAL
     C                   ADD       XXADJ         NEWBAL
     C                   ADD       XXREC         NEWBAL
     C                   ADD       XXPSTP        NEWBAL
     C                   ADD       XXCSTP        NEWBAL
     C                   SUB       XXPREV        NEWBAL
     C                   SUB       XXCREV        NEWBAL
     C                   ADD       XXPVOD        NEWBAL
     C                   ADD       XXCVOD        NEWBAL
     C                   SUB       XXCHG         NEWBAL
     C                   ADD       XXERR         NEWBAL
     C                   ADD       XXINT         NEWBAL
     C*
     C                   Z-ADD     NEWBAL        SCNBAL
     C*
     C* We have Passed Edits for Screen 2; Go To Next Screen.
     C*
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR03T        TAG
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C****************************************************************
     C* SRCALC    Subroutine to Calculate Check Reconciliation       *
     C****************************************************************
     C*
     C     SRCALC        BEGSR
     C*
     C* Check for deposits.
     C*
     C     CRRTP         IFEQ      'D'
     C                   ADD       CRCKAM        XXDEP
     C                   END
     C*
     C* INCLUDE CALCULATION FOR CHECKS ONLY IF THE 1ST RECON. DT = 0.
     C* THIS PREVENTS CHECKS FROM BEING INCLUDED TWICE IF WE HAVE A
     C* VOID OR STOP PAY IN A FUTURE RECONCILIATION PERIOD.
     C*
     C     CRRTP         IFEQ      'C'
     C     CRRDT1        ANDEQ     0
     C     CRRVFL        ANDEQ     'N'
     C                   ADD       CRCKAM        XXCHK
     C                   END
     C*
     C* Check for transfers.
     C*
     C     CRRTP         IFEQ      'T'
     C                   ADD       CRCKAM        XXTRN
     C                   END
     C*
     C* Check for NSF's.
     C*
     C     CRRTP         IFEQ      'N'
     C                   ADD       CRCKAM        XXNSF
     C                   END
     C*
     C* Check for adjustments.
     C*
     C     CRRTP         IFEQ      'A'
     C                   ADD       CRCKAM        XXADJ
     C                   END
     C*
     C* Check for recoveries.
     C*
     C     CRRTP         IFEQ      'R'
     C                   ADD       CRCKAM        XXREC
     C                   END
     C*
     C* Check for stop payments  - NEED TO CHECK ACTIVITY DATE FOR THE
     C* SITIUATION OF AN UNDO & THEN REDO.
     C*
     C     CRRTP         IFEQ      'C'
     C     CRACTP        ANDEQ     'S'
     C     CRACDT        ANDLE     XXTODT
     C     CRCKDT        IFLE      XXFRDT
     C                   ADD       CRCKAM        XXPSTP
     C                   ELSE
     C                   ADD       CRCKAM        XXCSTP
     C                   END
     C                   END
     C*
     C* INCLUDE CALC FOR REVERSALS ONLY IF THE 1ST RECON. DT = 0.
     C*
     C     CRRTP         IFEQ      'Z'
     C     CRRDT1        ANDEQ     0
     C     CRCKDT        IFLE      XXFRDT
     C                   ADD       CRCKAM        XXPREV
     C                   ELSE
     C                   ADD       CRCKAM        XXCREV
     C                   END
     C                   END
     C*
     C* Check for voids. - NEED TO CHECK ACTIVITY DATE FOR THE
     C* SITUATION OF AN UNDO & THEN REDO.
     C*
     C     CRRTP         IFEQ      'C'
     C     CRACTP        ANDEQ     'V'
     C     CRACDT        ANDLE     XXTODT
     C     CRCKDT        IFLE      XXFRDT
     C                   ADD       CRCKAM        XXPVOD
     C                   ELSE
     C                   ADD       CRCKAM        XXCVOD
     C                   END
     C                   END
     C*
     C* Check for bank charges.
     C*
     C     CRRTP         IFEQ      'B'
     C                   ADD       CRCKAM        XXCHG
     C                   END
     C*
     C* Check for bank errors.
     C*
     C     CRRTP         IFEQ      'E'
     C                   ADD       CRCKAM        XXERR
     C                   END
     C*
     C* Check for interest.
     C*
     C     CRRTP         IFEQ      'I'
     C                   ADD       CRCKAM        XXINT
     C                   END
     C*
     C                   ENDSR
