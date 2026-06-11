     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W4226R - Check Reconciliation File Update
     H*
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  05-12-93 004  PROJECT WC258                                  *
     H*                FORCE RELEASE RECORD DUE TO RECORD LOCK OCCUR- *
     H*                RING BAD DATA (UNEXPECTED DATA) IS FOUND       *
     H*                                                               *
     H* 12/08/95 C:22 PROJECT SUPPORT                                 *
     H*               CHANGED ACCUMUATION FOR BANK ERRORS             *
     H*                                                               *
     H*  09/04/97 C:30 Project: WC659                                 *
     H*           Version 3.  Replace WMBAP with WSCKP.               *
     H*                                                               *
     H*****************************************************************
     H*
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
      *** BEGIN CHG *** 09/04/97 ********************************
     F****WMBAL1  IF  E           K        DISK
      *
      *  Bank Account Master file
     FWSCKL     IF   E           K DISK
      *** END   CHG *** 09/04/97 ********************************
     FWMCSL     UF A E           K DISK
     FWDCRL2    UF   E           K DISK
     FWDCRL4    UF   E           K DISK
     F                                     RENAME(PCR01:PCR02)
     FWDCRL1    UF   E           K DISK
     F                                     RENAME(PCR01:PCR03)
     FWXCXL     O    E           K DISK

     D*
     D* System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* External Program Data Structure
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
      *** BEGIN CHG *** 09/04/97 ***********************************************
     C**Key List for the Bank Account Master File
     C*****
     C*****      BAKEY     KLIST
     C*****                KFLD           XXCO#   30
     C*****                KFLD           XXACCT 20
      *
      *  Bank Account Master file
      *
     C     CKKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXACCT           20
      *
      *** END   CHG *** 09/04/97 ***********************************************
     C*
     C* Key List for the Check Summary Master File
     C*
     C     CSKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXACCT           20
     C*
     C     CSKY1         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXACCT           20
     C                   KFLD                    XXTODT
     C*
     C* Key list for the Check Reconciliation Detail File
     C*
     C     CRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXACCT
     C                   KFLD                    XXFRDT
     C*
     C     CRKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXACCT
     C*
     C     CRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXACCT
     C                   KFLD                    XXRDT1
     C*
     C     CRKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXACCT
     C                   KFLD                    XXCHK#
     C*
     C     *LIKE         DEFINE    CRDT          XXFRDT
     C     *LIKE         DEFINE    CRDT          XXTODT
     C     *LIKE         DEFINE    CRDT          XXRDT1
     C     *LIKE         DEFINE    CRRVDT        XXRVDT
     C     *LIKE         DEFINE    CRRVTR        XXRVTR
     C     *LIKE         DEFINE    CRACDT        SVACDT
     C*
     C* Entry Parms
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQACCT           20
     C                   PARM                    QQTDT             8
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
     C* Set-Up and Initialize
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVEL     QQACCT        XXACCT           20
     C                   MOVE      QQTDT         X1TODT            8 0
     C*
     C                   Z-ADD     X1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   TIME                    DSDTTM
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
     C                   Z-ADD     0             CXDEP             9 2          Deposits
     C                   Z-ADD     0             CXCHK             9 2          Checks
     C                   Z-ADD     0             CXTRN             9 2          Transfers
     C                   Z-ADD     0             CXNSF             9 2          NSF's
     C                   Z-ADD     0             CXADJ             9 2          Adjustments
     C                   Z-ADD     0             CXREC             9 2          Recoveries
     C                   Z-ADD     0             CXPSTP            9 2          Stop pymts
     C                   Z-ADD     0             CXCSTP            9 2          Stop pymts
     C                   Z-ADD     0             CXPREV            9 2          Reverses
     C                   Z-ADD     0             CXCREV            9 2          Reverses
     C                   Z-ADD     0             CXPVOD            9 2          Voids
     C                   Z-ADD     0             CXCVOD            9 2          Voids
     C                   Z-ADD     0             CXCHG             9 2          Bank charges
     C                   Z-ADD     0             CXERR             9 2          Bank errors
     C                   Z-ADD     0             CXINT             9 2          Interest
     C*
     C* Get the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C   60              MOVE      *BLANK        UPUSNM
     C*
     C* Get the Company Record
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C* Get the Bank Account Master Record
     C*
      *** BEGIN CHG *** 09/04/97 *************************************
     C***********BAKEY     CHAINPBA01                60
      *
     C     CKKEY         CHAIN     PCK01                              60
      *
      *** END   CHG *** 09/04/97 *************************************
     C*
     C* GET THE LAST CHECK SUMMARY RECORD TO GET THE PREVIOUS ENDING
     C* BALANCE AND USE THE ENDING DATE AS THE BEGINNING DATE FOR THIS
     C* RECONCILIATION.  IF THERE ARE INACTIVE SUMMARY RECORDS WHICH
     C* HAVE AN ENDING DATE GREATER THAN ENTERED DATE, DELETE THE RCD.
     C*
     C     CSKEY         SETGT     PCS01                              60
     C                   READP     PCS01                                  60
     C     *IN60         DOWEQ     '0'
     C     CSSTAT        ANDEQ     'I'
     C     CSEDAT        IFGT      XXTODT
     C                   DELETE    PCS01
     C                   END
     C                   READP     PCS01                                  60
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C     CSCO#         ANDEQ     XXCO#
     C     CSACCT        ANDEQ     XXACCT
     C                   Z-ADD     CSEDAT        XXFRDT
     C                   Z-ADD     CSEBAL        OLDBAL            9 2
     C                   ELSE
     C                   Z-ADD     0             XXFRDT
     C                   Z-ADD     0             OLDBAL
     C                   END
     C*
     C* CALCULATE NEW BALANCE BY GETTING ALL RECORDS WHICH HAD ACTIVITY
     C* AFTER THE LAST ENDING BALANCE DATE AND UP TO AND INCLUDING THE
     C* NEW ENTERED ENDING DATE. INCLUDE ONLY THOSE RECORDS WHOSE
     C* 1ST RECONCILIATION DATE IS 0; OR
     C* WE HAVE A VOID OR STOP PAY WHICH DID NOT OCCUR IN THE SAME
     C* RECONCILIATION PERIOD AS THE CHECK.
     C*
     C* WE UPDATE PERMANENT STATUS = WITH ACTIVITY TYPE, AND MOVE
     C* THE NEW ENTERED ENDING DATE INTO THE 1ST RECONCILIATION DATE
     C* IF IT IS 0, OTHERWISE MOVE THE NEW ENDING DATE INTO THE 2ND
     C* RECONCILIATION DATE.
     C*
     C                   Z-ADD     DSTIME        XXTIME            6 0
     C     CRKEY         SETGT     PCR01                              60
     C     CRKEY1        READE     PCR01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     CRACDT        IFLE      XXTODT
     C*
     C*     IF CRRDT1 = 0, THEN THE RECORD WAS ENTERED & UPDATED IN THE
     C*     SAME RECONCILIATION PEROID. THEREFORE CALC & UPDATE CRRDT1
     C*
     C     CRRDT1        IFEQ      0
     C*
     C                   EXSR      SRCALC
     C                   Z-ADD     XXTODT        CRRDT1
     C*
     C                   ELSE
     C*
     C*     IF CRRDT1 NE 0, THEN WE ARE UPDATING A RECORD WHICH WAS
     C*     ENTERED IN A PRIOR PERIOD. ACCOUNT FOR VOIDS & STOP PAYS
     C*     THEN UPDATE THE 2ND RECONCILIATION DATE.
     C*
     C*     IF CRRDT1 NE 0 AND CRRDT2 NE 0, THEN WE HAVE THE SITUATION
     C*     IN WHICH A CHECK WAS ENTERED ONE PERIOD, STOPPED THE NEXT
     C*     PERIOD, AND THEN IN FOLLOWING PERIOD IS REVERSED AND
     C*     VOIDED.  NEED TO MOVE CRRDT2 TO CRRDT1.  DO NOT REPLACE
     C*     REVERSE FLAG & INFO UNLESS THE REVERSE OCCURRED PRIOR TO
     C*     CURRENT REC PERIOD.
     C*
     C     CRACTP        IFNE      'X'
     C                   EXSR      SRCALC
     C                   END
     C     CRRDT2        IFNE      0
     C                   Z-ADD     CRRDT2        CRRDT1
     C                   END
     C     CRRVTR        IFLE      XXFRDT
     C                   Z-ADD     XXTODT        CRRDT2
     C                   MOVE      'N'           CRRVFL
     C                   MOVE      ' '           CRRVTP
     C                   Z-ADD     0             CRRVDT
     C                   MOVE      ' '           CRSVST
     C                   Z-ADD     0             CRSVDT
     C                   Z-ADD     0             CRRVTR
     C                   END
     C*
     C                   END
     C*
     C* IF WE HAVE A REVERSE ENTRY, WE NEED TO UPDATE THE VOIDED OR
     C* STOPPED CHECK RECORD WITH THE REVERSAL INFORMATION AND THEN
     C* DELETE THE ACTUAL REVERSAL ENTRY.  (CRRTP = 'Z')
     C*
     C     CRRTP         IFNE      'Z'
     C*
     C                   MOVE      CRACTP        CRSTAT
     C                   MOVE      Q1USER        CRUSER
     C                   MOVE      Q1PGM         CRPGM
     C                   Z-ADD     XXTIME        CRTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRDT
     C*
     C                   UPDATE    PCR01
     C*
     C* WRITE THE RECORD TO THE WORK FILE FOR CHECK REC REPORT
     C*
     C                   MOVE      *BLANKS       CRRVFL                         JJJ 12-14-89
     C                   EXSR      SRWRIT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     CRCHK#        XXCHK#            7 0
     C                   MOVE      CRRVTP        XXRVTP            1
     C                   Z-ADD     CRRVDT        XXRVDT
     C                   Z-ADD     CRRVTR        XXRVTR
     C                   DELETE    PCR01
     C     CRKY3         CHAIN     PCR03                              60
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
     C     XXRVDT        ANDGT     XXFRDT
     C                   MOVE      CRACTP        SVACTP            1
     C                   MOVE      CRACDT        SVACDT
     C                   MOVE      XXRVTP        CRACTP
     C                   Z-ADD     XXRVDT        CRACDT
     C                   EXSR      SRCALC
     C                   EXSR      SRWRIT
     C                   MOVE      SVACTP        CRACTP
     C                   Z-ADD     SVACDT        CRACDT
     C                   END
     C*
     C*  SAVE ORIGINAL STATUS & REC DATE IN THE CASE OF "REC UNDO"
     C*
     C*  CASE 1: CHECK ENTERED ONE PERIOD, STOPPED THE NEXT. FOLLOWING
     C*          PERIOD ITS BEEN REVERSED ONLY.
     C*
     C     CRRDT2        IFNE      XXTODT
     C     CRRDT2        ANDNE     0
     C                   MOVE      CRSTAT        CRSVST
     C                   Z-ADD     CRRDT2        CRSVDT
     C                   END
     C*
     C*  CASE 2: CHECK ENTERED ONE PERIOD. STOPPED, REVERSED, AND THEN
     C*          VOIDED ALL IN A FOLLOWING PERIOD.
     C*
     C     CRRDT2        IFEQ      XXTODT
     C     XXRVDT        ANDGT     XXFRDT
     C                   MOVE      *BLANKS       CRSVST
     C                   Z-ADD     CRRDT1        CRSVDT
     C                   END
     C*
     C*  CASE 3: CHECK STOPPED ONE PERIOD. REVERSED & VOIDED BOTH IN A
     C*          FOLLOWING PERIOD.
     C*
     C     CRRDT2        IFEQ      XXTODT
     C     XXRVDT        ANDLE     XXFRDT
     C                   MOVE      XXRVTP        CRSVST
     C                   Z-ADD     CRRDT1        CRSVDT
     C                   END
     C*
     C*  CASE 4: CHECK ENTERED & STOPPED IN SAME PERIOD. REVERSED ONLY
     C*          IN A FOLLOWING PERIOD.
     C*
     C     CRRDT2        IFEQ      0
     C     CRRDT1        ANDLT     XXTODT
     C                   MOVE      CRSTAT        CRSVST
     C                   Z-ADD     CRRDT1        CRSVDT
     C                   END
     C*
     C                   MOVE      'Y'           CRRVFL
     C*
     C*  IF CHECK HAS NOT BEEN REVERSED & VOIDED IN SAME PERIOD, CLEAR
     C*  THE STATUS, ACTIVITY TYPE & DATE.
     C*
     C     CRACTP        IFEQ      XXRVTP
     C                   MOVE      *BLANKS       CRACTP
     C                   Z-ADD     0             CRACDT
     C                   MOVE      *BLANKS       CRSTAT
     C                   END
     C                   Z-ADD     XXTODT        CRRDT1
     C                   Z-ADD     0             CRRDT2
     C                   MOVE      XXRVTP        CRRVTP
     C                   Z-ADD     XXRVDT        CRRVDT
     C                   Z-ADD     XXRVTR        CRRVTR
     C                   MOVE      Q1USER        CRUSER
     C                   MOVE      Q1PGM         CRPGM
     C                   Z-ADD     XXTIME        CRTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRDT
     C*
     C                   UPDATE    PCR03
     C*
     C* WRITE THE RECORD TO THE WORK FILE FOR CHECK REC REPORT
     C*
     C                   EXSR      SRWRIT
     C*
     C*+++++ BGN ADD 5-12-93 +++++++++++++++++++++++++++++++++++++
     C*
     C                   ELSE
     C                   EXCEPT    PCR03E
     C*
     C*+++++ END ADD 5-12-93 ++++++++++++ C:04 +++++++++++++++++++
     C*
     C                   END
     C                   END
     C                   END
     C*
     C                   END
     C*
     C                   ADD       1             XXTIME
     C     CRKEY1        READE     PCR01                                  60
     C                   END
     C*
     C* We also need to get all outstanding detail records (CRRDT1 = 0)
     C* If an outstanding record was entered and also had activity
     C* (CRRDT1 = 0, & CRACTP NE ' ', & CRACDT NE 0) during this
     C* reconciliation time period, then CRRDT1 would no longer be 0
     C* because the above routine would have already updated CRRDT1 &
     C* CRRDT2.  Therefore chain with XXCO#, XXACCT, & CRRDT1 = 0.
     C* Only include records whose permanent status = ' ' & check date
     C* is less than or equal to the entered reconciliation date.
     C*
     C                   Z-ADD     0             XXRDT1
     C     CRKY2         CHAIN     PCR02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     CRSTAT        IFEQ      ' '
     C     CRCKDT        ANDLE     XXTODT
     C     CRRTP         ANDNE     'Z'
     C*
     C                   EXSR      SRCALC
     C                   Z-ADD     XXTODT        CRRDT1
     C                   MOVE      Q1USER        CRUSER
     C                   MOVE      Q1PGM         CRPGM
     C                   Z-ADD     XXTIME        CRTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRDT
     C*
     C                   UPDATE    PCR02
     C*
     C* WRITE THE RECORD TO THE WORK FILE FOR CHECK REC REPORT
     C*
     C                   EXSR      SRWRIT
     C*
     C                   END
     C*
     C                   ADD       1             XXTIME
     C     CRKY2         READE     PCR02                                  60
     C                   END
     C*
     C* WRITE THE SUMMARY RECORD TO THE WORK FILE
     C*
     C                   Z-ADD     XXCO#         CXCO#
     C                   MOVE      XXACCT        CXACCT
     C                   Z-ADD     0             CXCHK#
     C                   Z-ADD     0             CXCKDT
     C                   Z-ADD     0             CXCKAM
     C                   MOVE      *BLANKS       CXKY
     C                   MOVE      *BLANKS       CXTYPE
     C                   MOVE      *BLANKS       CXACTP
     C                   Z-ADD     0             CXACDT
     C                   Z-ADD     0             CXRDT1
     C                   Z-ADD     0             CXRDT2
     C                   MOVE      *BLANKS       CXRVFL
     C                   MOVE      *BLANKS       CXDESC
     C                   MOVE      *BLANK        CXSTAT
     C                   MOVE      Q1USER        CXUSER
     C                   MOVE      Q1PGM         CXPGM
     C                   Z-ADD     XXTIME        CXTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXDT
     C*
     C                   Z-ADD     XXDEP         CXDEP             9 2          Deposits
     C                   Z-ADD     XXCHK         CXCHK             9 2          Checks
     C                   Z-ADD     XXTRN         CXTRN             9 2          Transfers
     C                   Z-ADD     XXNSF         CXNSF             9 2          NSF's
     C                   Z-ADD     XXADJ         CXADJ             9 2          Adjustments
     C                   Z-ADD     XXREC         CXREC             9 2          Recoveries
     C                   Z-ADD     XXPSTP        CXPSTP            9 2          Stop pymts
     C                   Z-ADD     XXCSTP        CXCSTP            9 2          Stop pymts
     C                   Z-ADD     XXPREV        CXPREV            9 2          Reverses
     C                   Z-ADD     XXCREV        CXCREV            9 2          Reverses
     C                   Z-ADD     XXPVOD        CXPVOD            9 2          Voids
     C                   Z-ADD     XXCVOD        CXCVOD            9 2          Voids
     C                   Z-ADD     XXCHG         CXCHG             9 2          Bank charges
     C                   Z-ADD     XXERR         CXERR             9 2          Bank errors
     C                   Z-ADD     XXINT         CXINT             9 2          Interest
     C                   WRITE     PCX01
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
     C* IF AN INACTIVE SUMMARY RECORD EXISTS FOR THIS RECONCILIATION
     C* DATE THEN UPDATE THE RECORD AND MAKE THE STATUS ACTIVE, ELSE
     C* WRITE A RECORD TO THE CHECK SUMMARY FILE CONTAINING THE NEW
     C* ENDING DATE AND NEW ENDING BALNANCE.
     C*
     C     CSKY1         CHAIN     PCS01                              40
     C*
     C                   Z-ADD     XXCO#         CSCO#
     C                   MOVE      *BLANKS       CSACCT
     C                   MOVEL     XXACCT        CSACCT
     C                   Z-ADD     XXTODT        CSEDAT
     C                   Z-ADD     NEWBAL        CSEBAL
     C                   MOVE      *BLANKS       CSSTAT
     C                   MOVE      Q1USER        CSUSER
     C                   MOVE      Q1PGM         CSPGM
     C                   Z-ADD     DSTIME        CSTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CSDT
     C*
     C     *IN40         IFEQ      '1'
     C                   WRITE     PCS01
     C                   ELSE
     C                   UPDATE    PCS01
     C                   END
     C*
     C* End the Program
     C*
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
     C* ONLY INCLUDE CHECKS WITH A RECONCILIATION DATE OF 0 IN ORDER TO
     C* ACCOUNT FOR VOIDS AND STOP PAYS WHICH MAY BE ENTERED IN THE
     C* FUTURE. ALSO DO NOT INCLUDE CHECKS WHICH HAVE BEEN REVERSED
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
     C* Check for stop payments. NEED TO CHECK ACTIVITY DATE FOR THE
     C* SITUATION OF AN UNDO & THEN REDO.
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
     C* INCLUDE CALC FOR REVERSALS
     C*
     C     CRRTP         IFEQ      'Z'
     C     CRCKDT        IFLE      XXFRDT
     C                   ADD       CRCKAM        XXPREV
     C                   ELSE
     C                   ADD       CRCKAM        XXCREV
     C                   END
     C                   END
     C*
     C* Check for voids. NEED TO CHECK ACTIVITY DATE FOR THE SITUATION
     C* OF AN UNDO & THEN REDO.
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
     C*
     C****************************************************************
     C* SRWRIT  Subroutine to WRITE TO WORK FILE FOR CHECK REC REPORT*
     C*         WRITE A RECORD FOR EACH TRANSACTION WITHIN THE CHECK *
     C*         CHECK REC PERIOD. IF IT IS A VOID, STOP PAY, OR      *
     C*         REVERSAL WRITE A SECOND RECORD WITH CXTYPE MATCHING  *
     C*         THE TYPE OF TRANSACTION (V,S, OR Z)                  *
     C****************************************************************
     C*
     C     SRWRIT        BEGSR
     C*
     C* WRITE THE TRANSACTION RECORD
     C*
     C                   Z-ADD     CRCO#         CXCO#
     C                   MOVE      CRACCT        CXACCT
     C                   Z-ADD     CRCHK#        CXCHK#
     C                   Z-ADD     CRCKDT        CXCKDT
     C                   Z-ADD     CRCKAM        CXCKAM
     C                   MOVE      CRKY          CXKY
     C                   MOVE      CRRTP         CXTYPE
     C                   MOVE      CRACTP        CXACTP
     C                   Z-ADD     CRACDT        CXACDT
     C                   Z-ADD     CRRDT1        CXRDT1
     C                   Z-ADD     CRRDT2        CXRDT2
     C                   MOVE      CRRVFL        CXRVFL
     C                   MOVE      CRDESC        CXDESC
     C                   MOVE      *BLANK        CXSTAT
     C                   MOVE      Q1USER        CXUSER
     C                   MOVE      Q1PGM         CXPGM
     C                   Z-ADD     XXTIME        CXTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXDT
     C*
     C                   WRITE     PCX01
     C*
     C* WRITE ANOTHER RECORD FOR VOIDS, STOP PAYS, AND REVERSALS
     C* ONLY IF ACTIVITY OCCURRED DURING THIS REC. PERIOD
     C*
     C     CRACTP        IFEQ      'V'
     C     CRACTP        OREQ      'S'
     C     CRRVFL        OREQ      'Y'
     C*
     C     CRACDT        IFLE      XXTODT
     C*
     C     CRRVFL        IFEQ      'Y'
     C                   MOVE      'Z'           CXTYPE
     C                   ELSE
     C                   MOVE      CRACTP        CXTYPE
     C                   END
     C                   WRITE     PCX01
     C*
     C                   END
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     OPCR03     E            PCR03E
