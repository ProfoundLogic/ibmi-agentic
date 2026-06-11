     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W8021R1 - POST NET DEDUCTIBLE RECOVERY - UPDATE
     H*****************************************************************
     H*                                                               *
     H*   PROGRAM DESCRIPTION:                                        *
     H*     THIS PROGRAM PROCESSES TRANSACTIONS IN THE DEDUCTIBLE     *
     H*     BILLING OVERRIDE FILE FOR THE REQUESTED CHECK.  FOR       *
     H*     EACH TRANSACTION, THE RECOVERIES ARE POSTED AND THE       *
     H*     MASTER FILES ARE UPDATED.  THE DEDUCTIBLE BILLS FILE IS   *
     H*     UPDATED, THE OVERRIDE RECORD IS DELETED OR UPDATED, AND   *
     H*     THE PENDING RECOVERY RECORD IS DELETED.                   *
     H*                                                               *
     H*   CALLED FROM:                                                *
     H*     W8021C - POST NET DEDUCTIBLE RECOVERIES - SUBMITTING CL   *
     H*                                                               *
     H*   CALLS TO:                                                   *
     H*     NONE                                                      *
     H*                                                               *
     H*   PROGRAMMER: C:24                                            *
     H*   CREATED:    03/01/95    PROJECT:WC311                       *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 08/15/17 C:33  Project: SY394                                 *
     H*                Update initial reserve flag                    *
     H*                                                               *
     H* 12/09/15 C:136 Project: WC2393                                *
     H*                Change sort on WADDL1 so that negative billed  *
     H*                amounts are recovered first by default.        *
     H*                                                               *
     H* 08/29/11 C:277 SUPPORT 77320                                  *
     H*                Populate WDPAP initial fields                  *
     H*                                                               *
     H* 03/03/10 C:232 Project:  139WNH                               *
     H*                Update all of the initial maintenance fields.  *
     H*                Populating RHIDT and RHDT with the current     *
     H*                system date.                                   *
     H*                                                               *
     H* 08/09/07  C:191 PROJECT:WC1453                                *
     H*                 EFT PAYMENTS TO CLAIMANTS/PROVIDERS           *
     H*
     H* 12/22/05 C:15  Project:  139WAI                               *
     H*                Added code to load a deduct recovery record    *
     H*                to file VADUM2 instead of WDPAP.  This mod is  *
     H*                conditioned on the client #.                   *
     H*                                                               *
     H* 05/17/04 C:53  Project:  WC1186                               *
     H*                Load PAFEIN w/Employer FEIN and write WDRPP.   *
     H*                                                               *
     H* 03/23/04 C:15  Project:  WC1170                               *
     H*                Added code to load the reserve history buckets *
     H*                according to the order in the claim file.      *
     H*                                                               *
     H* 05/01/01 C:15  Project: WC1020                                *
     H*                Added code to retrieve the bank code for mult  *
     H*                bank account processing.                       *
     H*                                                               *
     H* 04/30/99 C:15 Support: WC100/17801                            *
     H*               Added code to load additional fields in WDPAP.  *
     H*                                                               *
     H* 02/02/99 C:15 Support: WC065/15291                            *
     H*               Changed pgm to always delete the deductible     *
     H*               workfile record.                                *
     H*                                                               *
     H* 04/09/98 C:65 PROJECT: WC812                                  *
     H*               ALLOW A PARTIAL CHECK AMOUNT TO BE RESOLVED,    *
     H*               AND APPLY TO REMAINING AMOUNT INSTEAD OF AMT.   *
     H*                                                               *
     H* 04/01/98 C:65 PROJECT: WC618                                  *
     H*            1) CHANGE THE FOLLOWING KEY LISTS TO USE THE CLAIM *
     H*               FYR INSTEAD OF THE BILLING FYR:  APKY, APKY1,   *
     H*               RRKY, AND RRKY1                                 *
     H*            2) If the claim is closed, the remaining reserves  *
     H*               will not be updated and a Reserve History record*
     H*               will be written.                                *
     H*                                                               *
     H* 05/13/97 C:62 PROJECT WC674                                   *
     H*               ENSURE ALL PROGRAMS USE CMLTFL = I/M/R NOT Y/N  *
     H*                                                               *
     H* 09/16/96 C:33 PROJECT: WC585                                  *
     H*               CHANGED PGM TO ALLOW THE USERS TO APPLY PART OF *
     H*               A RECOVERY CHECK.  TO ACCOMPLISH THIS, THE WADQP*
     H*               RECORD WILL HAVE TO REFLECT THE TOTAL CHECK AMT *
     H*               MINUS THE AMT RECOVERED.                        *
     H*                                                               *
     H* 06/13/96 C:15 PROJECT: WC521                                  *
     H*               IF THE PENDING RECOVERY AMOUNT IS GREATER THAN  *
     H*               THE CLAIM PMTS, THE AMOUNT RECOVERED IN THE     *
     H*               DEDUCT DETAIL FILE SHOULD REFLECT THE ACTUAL    *
     H*               AMOUNT APPLIED TO THE CLAIM.                    *
     H*                                                               *
     H* 03/09/95 C:15 PROJECT: WC413                                  *
     H*               IF THE PENDING RECOVERY AMOUNT IS GREATER THAN  *
     H*               THE CLAIM PMTS, DISSOLVE THE CLAIM PMTS TO 0    *
     H*               AND CREATE OR UPDATE THE WADQP ADJUSTMENT       *
     H*               RECORD WITH THE DIFFERENCE.                     *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*   50  -  TRANSACTION RECORD ACCESS LOOP CONTROL               *
     H*   60  -  GENERAL WORK INDICATOR                               *
     H*****************************************************************
     FWMFNL     IF   E           K DISK
     F*** BEGIN CHG ***  04/09/98  ************************************
     F***WADQL   UF  E           K        DISK                      A
     FWADQL     UF   E           K DISK
     F*** END CHG *****  04/09/98  ************************************
     FWADDL1    UF   E           K DISK
     FWDDDL     UF   E           K DISK
     F                                     RENAME(PDD01:PDD02)
     FWMCML1    UF   E           K DISK
     FWDELL     UF   E           K DISK
     F*** BEG ADD *** 05/17/04 ****************************
     FWMEML     IF   E           K DISK
     FWDRPP     O    E           K DISK
     F*** END ADD *** 05/17/04 ****************************
     FWMRHP     O    E           K DISK
     FWDPAL     IF A E           K DISK
     FWMRRL     IF A E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PRC01)
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
     F*** Begin add *** 12/22/05 **************************************
     FVADUM2    O    E             DISK
     F*** End add ***** 12/22/05 **************************************

     D*
     D*** Begin add *** 03/23/04 **************************************
     D*** End add ***** 03/23/04 **************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D*
     D*** Begin add *** 05/01/01 **************************************
     D                 DS
     D*** BEG CHG ***** 08/09/07 **************************************
     D***********                             1  50 DATA
     D  DATA                   1     75
     D*** END CHG ***** 08/09/07 **************************************
     D  QQTRDT                 1      7  0
     D  QQFRDT                 8     14  0
     D  QQTODT                15     21  0
     D  QQTRCD                22     23
     D  QQRECD                24     25
     D  QQPV1                 26     27
     D  QQPV2                 28     29
     D*** BEG ADD ***** 08/09/07 **************************************
     D  QQSEQ#                30     34
     D  QQEOB#                35     43
     D  QQFEIN                44     52  0
     D  QQMOD                 53     55  0
     D  QQDSQ#                56     60  0
     D*** END ADD ***** 08/09/07 **************************************
     D*** End add ***** 05/01/01 **************************************
     D*
     D*** Begin add *** 03/23/04 **************************************
     D                 DS                  INZ
     D  RRT                    1     20
     D                                     DIM(10)                              RH rsv types
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS                  INZ
     D  RSU                    1     90  2
     D                                     DIM(10)                              RH rsv amt
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
     D*** End add ***** 03/23/04 **************************************
     D*
     D XXDESC          C                   CONST('DEDUCTIBLE RECOVERY -
     D                                     RECEIVED')
      //***  Begin Add  ***  08/15/17  ************************************
      /copy wPrClmRsv
      //***  End   Add  ***  08/15/17  ************************************
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  FUND MASTER FILE KEY
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  DEDUCTIBLE PAYMENT RECOVERIES PENDING FILE KEY
     C*
     C     DQKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCHK#
     C*
     C*  DEDUCTIBLE DETAIL BILLS FILE KEY
     C*
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    DDFYR
     C                   KFLD                    DDCASE
     C                   KFLD                    DDBFYR
     C                   KFLD                    DDPODT
     C                   KFLD                    DDRECD
     C*
     C     DDKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  CLAIM MASTER FILE KEY
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    DDFYR
     C                   KFLD                    DDCASE
     C*
     C*  EMPLOYER DETAIL FILE KEY
     C*
     C     ELKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*
     C*  A/P TRANSACTION FILE KEY
     C*
     C     APKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** BEGIN CHG  ***  04/01/98  ***********************************
     C*****                KFLD           XXFYR
     C                   KFLD                    DDFYR
     C*** END CHG ******  04/01/98  ***********************************
     C                   KFLD                    DDCASE
     C                   KFLD                    XXSEQ             5 0
     C*
     C     APKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** BEGIN CHG  ***  04/01/98  ***********************************
     C*****                KFLD           XXFYR
     C                   KFLD                    DDFYR
     C*** END CHG ******  04/01/98  ***********************************
     C                   KFLD                    DDCASE
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  REQUEST REFUND FILE KEY
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** BEGIN CHG  ***  04/01/98  ***********************************
     C*****                KFLD           XXFYR
     C                   KFLD                    DDFYR
     C*** END CHG ******  04/01/98  ***********************************
     C                   KFLD                    DDCASE
     C                   KFLD                    XXREQ#            5 0
     C*
     C     RRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** BEGIN CHG  ***  04/01/98  ***********************************
     C*****                KFLD           XXFYR
     C                   KFLD                    DDFYR
     C*** END CHG ******  04/01/98  ***********************************
     C                   KFLD                    DDCASE
     C*
     C     *LIKE         DEFINE    DQCO#         XXCO#
     C     *LIKE         DEFINE    DQFND         XXFND
     C     *LIKE         DEFINE    DQFYR         XXFYR
     C     *LIKE         DEFINE    DQEMP#        XXEMP#
     C     *LIKE         DEFINE    DQDIV         XXDIV
     C     *LIKE         DEFINE    DQCHK#        XXCHK#
     C     *LIKE         DEFINE    DDAMTP        XXAMTP
     C     *LIKE         DEFINE    CMLTFL        XXLTFL
     C     *LIKE         DEFINE    DDAMTP        XXDIFF
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQCHK#            7
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
     C*** Begin add *** 12/22/05 **************************************
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** End add ***** 12/22/05 **************************************
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQCHK#        XXCHK#
     C*
     C*** Begin add *** 12/22/05 **************************************
     C*  Retrieve the client #.
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** End add ***** 12/22/05 **************************************
     C*
     C*  IF PROCESSING THE WADQP ADJUSTMENT RECORD, SETON *IN21.
     C*
     C     XXCHK#        COMP      9999999                                21
     C*
     C*  RETRIEVE THE FUND MASTER RECORD
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C*  GET THE DEDUCTIBLE RECOVERY PENDING RECORD AND UPDATE STATUS
     C*
     C     DQKY          CHAIN     PDQ01                              60
     C                   MOVE      'E'           DQSTAT
     C  N60              UPDATE    PDQ01
     C*
     C*  LOOP THROUGH ALL TRANSACTION RECORDS
     C*
     C     DDKY2         CHAIN     PDD01                              50
     C     *IN50         DOWEQ     *OFF
     C*
     C*  UPDATE THE AMOUNT RECOVERED IN THE TRANSACTION RECORD.
     C*  DELETE THE TRANSACTION IF THE AMOUNT RECOVERED EQUALS
     C*  THE AMOUNT BILLED.
     C*
     C                   Z-ADD     DDAMTP        XXAMTP
     C                   Z-ADD     *ZERO         DDAMTP
     C                   ADD       XXAMTP        DDAMTR
     C*** BEGIN DLT *** 02/02/99 **************************************
     C********** DDAMTR    IFEQ DDAMTB
     C*** END DLT ***** 02/02/99 **************************************
     C                   DELETE    PDD01
     C*** BEGIN DLT *** 02/02/99 **************************************
     C**********           ELSE
     C**********           UPDATPDD01
     C**********           ENDIF
     C*** END DLT ***** 02/02/99 **************************************
     C*
     C*
     C*  UPDATE THE RESERVES AND PAYMENTS IN THE CLAIM MASTER
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             X                 3 0
     C     DDRECD        LOOKUP    RT(X)                                  60
     C     *IN60         IFEQ      *OFF
     C     *BLANK        LOOKUP    RT(X)                                  60
     C                   MOVE      DDRECD        RT(X)
     C                   ENDIF
     C*
     C*  IF THE PENDING RECOVERY AMT IS GREATER THAN THE CLAIM PMTS,
     C*  DISSOLVE THE PMTS TO ZERO AND CREATE OR UPDATE THE WADQP
     C*  ADJUSTMENT RECORD WITH THE DIFFERENCE.  THE ADJUSTMENT RECORD
     C*  WILL BE DEFINED WITH CHECK# 9999999.
     C*
     C     XXAMTP        IFGT      AR(X)
     C     XXAMTP        SUB       AR(X)         XXDIFF
     C                   Z-ADD     AR(X)         XXAMTP
     C*
     C*** BEGIN DEL ***  04/09/98  ************************************
     C*****                Z-ADD0         DQAMT
     C*****                Z-ADD9999999   XXCHK#
     C*****      DQKY      CHAINPDQ01                40
     C*****                ADD  XXDIFF    DQAMT
     C*****                Z-ADDXXCHK#    DQCHK#
     C*****                MOVE Q1USER    DQUSER
     C*****                MOVE Q1PGM     DQPGM
     C*****                TIME           DSDTTM
     C*
     C*****                Z-ADDDSDATE    SRDTI
     C*****                CALL 'SRCVTDAT'DTCYMD
     C*****                MOVELSRDTO     DQDT
     C*
     C*****                Z-ADDDSTIME    DQTIME
     C**N21                MOVE *BLANK    DQSTAT
     C**N40                UPDATPDQ01
     C***40                WRITEPDQ01
     C*** END DEL *****  04/09/98  ************************************
     C*
     C                   END
     C*
     C*** BEGIN ADD *** 02/02/99 **************************************
     C*  Only update the files if the amount is NOT zero.
     C*
     C     XXAMTP        IFNE      0
     C*** END ADD ***** 02/02/99 **************************************
     C*
     C*** BEGIN ADD ***  04/01/98  ************************************
     C     CMSTAT        IFEQ      'O'
     C                   ADD       XXAMTP        RR(X)
     C                   ENDIF
     C*** END ADD *****  04/01/98  ************************************
     C                   SUB       XXAMTP        AR(X)
     C*** Begin add *** 12/22/05 **************************************
     C     CLTID         IFNE      '139'
     C*** End add ***** 12/22/05 **************************************
     C                   UPDATE    PCM01
     C*** Begin add *** 12/22/05 **************************************
     C                   ENDIF
     C*** End add ***** 12/22/05 **************************************
     C*
     C*  UPDATE THE AMOUNT RECOVERED AND AMOUNT PENDING IN THE
     C*  DEDUCTIBLES BILLED RECORD.
     C*
     C     DDKY          CHAIN     PDD02                              60
     C                   ADD       XXAMTP        DDAMTR
     C                   SUB       XXAMTP        DDAMTP
     C  N60              UPDATE    PDD02
     C*
     C*  SUBTRACT THE RECOVERED AMOUNT FROM THE DEDUCT RECOVERY
     C*  PENDING FILE.
     C*
     C                   MOVE      QQCHK#        XXCHK#
     C     DQKY          CHAIN     PDQ01                              60
     C*** BEGIN CHG ***  04/09/98  ************************************
     C*****                SUB  XXAMTP    DQAMT
     C*****                SUB  XXDIFF    DQAMT
     C                   SUB       XXAMTP        DQAMTR
     C*** END CHG *****  04/09/98  ************************************
     C  N60              UPDATE    PDQ01
     C*
     C* * UPDATE THE EMPLOYER DETAIL FILE WITH THE OUTSTANDING
     C* * RESERVE & TOTAL CLAIMS PAID.
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   SUB       XXAMTP        ELTCLP
     C*
     C*** BEGIN ADD ***  04/01/98  ************************************
     C     CMSTAT        IFEQ      'O'
     C                   ADD       XXAMTP        ELOUTS
     C                   ENDIF
     C*** END ADD *****  04/01/98  ************************************
     C                   MOVE      Q1USER        ELUSER
     C                   MOVE      Q1PGM         ELPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C*
     C                   Z-ADD     DSTIME        ELTIME
     C                   UPDATE    PEL01
     C                   END
     C*
     C*  WRITE THE RESERVE HISTORY RECORD
     C*
     C*** BEGIN ADD ***  04/01/98  ************************************
     C     CMSTAT        IFEQ      'C'
     C*** END ADD *****  04/01/98  ************************************
     C*** Begin add *** 12/22/05 **************************************
     C     CLTID         ANDNE     '139'
     C*** End add ***** 12/22/05 **************************************
     C                   CLEAR                   PRH01
     C*
     C                   MOVE      CMCO#         RHCO#
     C                   MOVE      CMFND         RHFND
     C                   Z-ADD     CMFYR         RHFYR
     C                   MOVE      CMCASE        RHCASE
     C*** Begin dlt *** 03/23/04 **************************************
     C**********           MOVE RT,X      RHRT01
     C**********           SUB  XXAMTP    RHSU01
     C*** End dlt ***** 03/23/04 **************************************
     C*** Begin add *** 03/23/04 **************************************
     C                   MOVE      RT(X)         RRT(X)
     C                   Z-SUB     XXAMTP        RSU(X)
     C*** End add ***** 03/23/04 **************************************
     C                   MOVEL     XXDESC        RHEXPL
     C                   MOVE      *BLANKS       RHSTAT
     C                   MOVE      *BLANKS       RHCLST
     C                   MOVE      Q1USER        RHUSER
     C                   MOVE      Q1PGM         RHPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RHDT
     C*
     C                   MOVE      RHDT          RHEFDT
     C                   Z-ADD     DSTIME        RHTIME
     C*
     C*** BEG ADD ***** 03/03/10 **************************************
     C                   MOVEL     SRDTO         RHIDT
      *
     C                   MOVE      Q1USER        RHIUSR
     C                   MOVE      Q1PGM         RHIPGM
      *
     C                   Z-ADD     DSTIME        RHITIM
      *
     C*** END ADD ***** 03/03/10 **************************************
      //***  Begin Add  ***  08/15/17  ************************************
      /Free
       RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);
      /End-free
      //***  End   Add  ***  08/15/17  ************************************
     C*
     C                   WRITE     PRH01
     C*** BEGIN ADD ***  04/01/98  ************************************
     C                   ENDIF                                                  CMSTAT IFEQ
     C*** END ADD *****  04/01/98  ************************************
     C*
     C*** BEGIN ADD *** 02/02/99 **************************************
     C                   END
     C*** END ADD ***** 02/02/99 **************************************
     C*
     C                   END
     C*
     C*** BEGIN ADD *** 02/02/99 **************************************
     C*  Only update the files if the amount is NOT zero.
     C*
     C     XXAMTP        IFNE      0
     C*** END ADD ***** 02/02/99 **************************************
     C*** Begin add *** 12/22/05 **************************************
     C     CLTID         IFNE      '139'
     C*** End add ***** 12/22/05 **************************************
     C*
     C*  WRITE THE PAYMENT DETAIL RECORD
     C*
     C                   Z-ADD     99999         XXSEQ
     C     APKY          SETGT     PPA01
     C     APKY1         READPE    PPA01                                  60
     C   60              Z-ADD     *ZERO         PASEQ#
     C                   ADD       1             PASEQ#
     C                   MOVE      XXCO#         PACO#
     C                   MOVE      XXFND         PAFND
     C                   MOVE      DDFYR         PAFYR
     C                   MOVE      DDCASE        PACASE
     C                   Z-ADD     XXCHK#        PACHK#
     C                   Z-ADD     *ZERO         PABASQ
     C*** BEG ADD *** 05/17/04 ****************************
     C     DDKY2         CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFEIN        PAFEIN
     C                   ELSE
     C*** END ADD *** 05/17/04 ****************************
     C                   MOVE      *ZERO         PAFEIN
     C*** BEG ADD *** 05/17/04 ****************************
     C                   ENDIF
     C*** END ADD *** 05/17/04 ****************************
     C                   MOVE      *ZERO         PAMOD
     C                   MOVE      XXAMTP        PAAMT
     C*
     C*  GET THE RESERVE CODE TYPE TO RETRIEVE THE TRANSACTION
     C*  TYPE AND PAYMENT VALUE 1 FROM THE FUND MASTER
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     DDRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C     RTTYPE        IFEQ      'M'
     C                   MOVE      FNMEP1        PABR2
     C                   MOVE      FNMETR        PATRCD
     C                   ELSE
     C     RTTYPE        IFEQ      'C'
     C                   MOVE      FNCOP1        PABR2
     C                   MOVE      FNCOTR        PATRCD
     C                   ELSE
     C     RTTYPE        IFEQ      'E'
     C                   MOVE      FNEXP1        PABR2
     C                   MOVE      FNEXTR        PATRCD
     C                   ELSE
     C     RTTYPE        IFEQ      'L'
     C                   MOVE      FNLEP1        PABR2
     C                   MOVE      FNLETR        PATRCD
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*  FIND THE FIRST COMPATIBLE PAYMENT VALUE 2 BASED ON
     C*  LOST TIME FLAG.
     C*
     C*** BEGIN DEL *** 05/13/97 **************************************
     C********** CMLTFL    IFEQ 'L'
     C*** END   DEL *** 05/13/97 **************************************
     C*** BEGIN ADD *** 05/13/97 **************************************
     C     CMLTFL        IFEQ      'I'
     C*** END   ADD *** 05/13/97 **************************************
     C                   MOVE      'Y'           XXLTFL
     C                   ELSE
     C                   MOVE      'N'           XXLTFL
     C                   END
     C*
     C     XXCO#         CHAIN     PB701                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     B7LTLF        IFEQ      XXLTFL
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C  N60XXCO#         READE     PB701                                  60
     C                   ENDDO
     C*
     C                   MOVEL     B7KEY         PABR7
     C*
     C                   Z-ADD     *ZERO         PAFRDT
     C                   Z-ADD     *ZERO         PATODT
     C                   Z-ADD     *ZERO         PAWKS
     C                   Z-ADD     *ZERO         PADAYS
     C                   MOVE      'R'           PATYPE
     C*
     C*  GET THE RESERVE CODE AND TYPE FOR THE TRANSACTION CODE
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     PATRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C                   MOVE      PCRECD        PARECD
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     PCRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C                   MOVE      RTTYPE        PARETP
     C*
     C*  LOAD THE DESCRIPTION FROM PAYMENT VALUE 1
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     PABR2         XXKEY
     C     TBKY          CHAIN     PB201                              60
     C                   MOVEL     B2DESC        PADESC
     C                   MOVE      *BLANK        PASTAT
     C                   MOVE      *BLANK        PAADST
     C                   Z-ADD     0             PABUL1
     C                   MOVE      Q1USER        PAUSER
     C                   MOVE      Q1PGM         PAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATRDT
     C*
     C                   MOVE      PATRDT        PAPODT
     C                   MOVE      PATRDT        PADT
     C                   Z-ADD     DSTIME        PATIME
     C*** BEGIN ADD *** 08/29/11 *****************************************
     C                   MOVE      Q1USER        PAIUSR
     C                   MOVE      Q1PGM         PAIPGM
     C                   MOVEL     SRDTO         PAIDT
     C                   Z-ADD     DSTIME        PAITIM
     C*** END ADD *** 08/29/11 *******************************************
     C*** BEGIN ADD *** 04/30/99 **************************************
     C                   MOVE      *BLANKS       PAPGTP
     C                   Z-ADD     0             PAEOB#
     C                   MOVE      *BLANKS       PACKTP
     C*
     C*  Call API to access "Current" Bank Account
     C*
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         ##SRDT                         MMDDCCYY
     C*
     C*** Begin add *** 05/01/01 **************************************
      *  Call API to determine the Bank "Code".
      *
     C                   MOVE      PAFYR         ##FYR
     C                   MOVE      PACASE        ##CASE
     C                   MOVEL     PATRDT        QQTRDT
     C                   MOVEL     PATRDT        QQFRDT
     C                   MOVEL     PATRDT        QQTODT
     C                   MOVEL     PATRCD        QQTRCD
     C                   MOVEL     PARECD        QQRECD
     C                   MOVEL     PABR2         QQPV1
     C                   MOVEL     PABR7         QQPV2
     C*** BEG ADD ***** 08/09/07 **************************************
     C                   MOVE      *BLANKS       QQSEQ#
     C                   MOVE      *ZEROS        QQDSQ#
     C                   MOVE      *BLANKS       QQEOB#
     C                   MOVE      *ZEROS        QQFEIN
     C                   MOVE      *ZEROS        QQMOD
     C*** END ADD ***** 08/09/07 **************************************
      *
     C                   CALL      'WA023R1'
     C                   PARM      '2'           ##MODE            1            Mode
     C                   PARM                    ##CO#                          Company
     C                   PARM                    ##FND                          Group #
     C                   PARM                    ##FYR             3            Year
     C                   PARM                    ##CASE            7            Case
     C                   PARM      'WC'          ##APPL                         Applic Code
      ***** BEG CHG ***** 08/09/07 ************************************
     C*******              PARM DATA      ##DATA 50        Selection criteria
     C                   PARM      DATA          ##DATA           75            Selection criteria
      ***** END CHG ***** 08/09/07 ************************************
     C                   PARM                    ##CKTP           10            Bank Acct Code
     C                   PARM                    ##RTCD                         Return Code
     C*** End add ***** 05/01/01 **************************************
     C*
     C                   CALL      'WA023R'
     C                   PARM                    ##CO#             3            Company
     C                   PARM                    ##FND             3            Group #
     C                   PARM      'WC'          ##APPL            2            Applic Code
     C                   PARM                    ##SRDT            8            MMDDCCYY
     C                   PARM      *BLANK        ##ACCT           20            Bank Acct #
     C                   PARM      'C'           ##CALL            1            (V)oucher/(C)heck
     C                   PARM      *BLANK        ##HDCY            1            Hardcopy Y/N
     C                   PARM      *BLANK        ##CHKQ           10            Check/Voucher OUTQ
     C                   PARM      *BLANK        ##FTYP           10            Form Type
     C                   PARM      *BLANK        ##RTCD            1            Return Code
     C                   PARM      *ZERO         ##EFDT            7            WMBIP Eff. DT CYYMMD
     C*** Begin add *** 05/01/01 **************************************
     C                   PARM      *BLANKS       ##CPYQ           10
     C                   PARM                    ##CKTP
     C*** End add ***** 05/01/01 **************************************
     C*
     C                   MOVE      ##ACCT        PAACCT
     C*** Begin add *** 05/01/01 **************************************
     C                   MOVE      ##CKTP        PACKTP
     C*** End add ***** 05/01/01 **************************************
     C*
     C                   MOVE      CMOFF#        PAOFF#
     C                   Z-ADD     0             PARCVD
     C                   Z-ADD     0             PADUE
     C                   MOVE      *BLANKS       PALTCD
     C                   Z-ADD     0             PADPCD
     C                   MOVE      *BLANKS       PADPCU
     C                   MOVE      Q1USER        PARQST
     C                   MOVE      *BLANKS       PAFNAL
     C                   Z-ADD     0             PARCDT
     C                   MOVE      *BLANKS       PARADJ
     C                   Z-ADD     0             PAEOBS
     C*** END ADD ***** 04/30/99 **************************************
     C*** BEG ADD *** 05/17/04 ****************************
     C*
     C* Write check detail rcd
     C*
     C                   CLEAR                   PRP01
     C*
     C                   Z-ADD     PACO#         RPCO#
     C                   Z-ADD     PAFND         RPFND
     C                   Z-ADD     PAFYR         RPFYR
     C                   Z-ADD     PACASE        RPCASE
     C                   Z-ADD     PACHK#        RPCHK#
      *
     C                   MOVEL     EMNAME        RPPNAM
     C*
     C     EMMPFL        IFEQ      'M'
     C                   MOVEL     EMMAD1        RPADR1
     C                   MOVEL     EMMAD2        RPADR2
     C                   MOVEL     EMMCTY        RPCTY
     C                   MOVE      EMMST         RPST
     C                   MOVEL     EMMZIP        RPZIP
     C                   MOVE      EMMZIP        RPZIP
     C                   ELSE
     C                   MOVEL     EMPAD1        RPADR1
     C                   MOVEL     EMPAD2        RPADR2
     C                   MOVEL     EMPCTY        RPCTY
     C                   MOVE      EMPST         RPST
     C                   MOVEL     EMPZIP        RPZIP
     C                   MOVE      EMPZIP        RPZIP
     C                   ENDIF
     C*
     C                   Z-ADD     PATRDT        RPTRDT
     C                   Z-ADD     PASEQ#        RPSEQ#
     C*
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
     C                   Z-ADD     PADT          RPDT
     C                   Z-ADD     PATIME        RPTIME
     C*
     C                   WRITE     PRP01
     C*** END ADD *** 05/17/04 ****************************
     C*
     C                   WRITE     PPA01
     C*
     C*  FIND THE NEXT AVAILABLE REQUEST NUMBER
     C*
     C                   Z-ADD     99999         XXREQ#
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     RRREQ#        ADD       1             RRREQ#
     C                   ELSE
     C                   Z-ADD     1             RRREQ#
     C                   END
     C*
     C                   Z-ADD     DDFND         RRFND
     C                   Z-ADD     DDFYR         RRFYR
     C                   Z-ADD     DDCO#         RRCO#
     C                   Z-ADD     DDCASE        RRCASE
     C                   Z-ADD     *ZERO         RRFEIN
     C                   Z-ADD     *ZERO         RRMOD
     C                   MOVE      PATRDT        RRDTRC
     C                   Z-ADD     XXAMTP        RRRCVD
     C                   Z-ADD     0             RRRAMT
     C                   MOVEL     PADESC        RRDESC
     C                   MOVE      Q1USER        RRUSER
     C                   MOVE      Q1PGM         RRPGM
     C                   TIME                    DSDTTM
     C                   MOVE      PATRDT        RRDT
     C                   Z-ADD     DSTIME        RRTIME
     C*
     C                   MOVE      'A'           RRSTAT
     C                   WRITE     PRR01
     C*
     C*** Begin add *** 12/22/05 **************************************
     C                   ELSE
     C*
     C*  For client 139, write a record to file VADUM2.
     C*
     C                   MOVEL     CMPDS#        VACLM#
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         VATRDT
     C                   MOVE      DDRECD        VARECD
     C                   MOVE      DQCHK#        VACHK#
     C                   MOVE      XXAMTP        VAAMT
     C     XXAMTP        IFLT      0
     C                   MOVE      '-'           VAAMTS
     C                   ENDIF
     C*
     C                   MOVEL(P)  DDRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C                   SELECT
     C     RTTYPE        WHENEQ    'M'
     C                   MOVE      FNMEP1        VABR2
     C     RTTYPE        WHENEQ    'C'
     C                   MOVE      FNCOP1        VABR2
     C     RTTYPE        WHENEQ    'E'
     C                   MOVE      FNEXP1        VABR2
     C     RTTYPE        WHENEQ    'L'
     C                   MOVE      FNLEP1        VABR2
     C                   ENDSL
     C*
     C                   WRITE     PVA01
     C*
     C                   ENDIF
     C*** End add ***** 12/22/05 **************************************
     C*
     C*** BEGIN ADD *** 02/02/99 **************************************
     C                   END
     C*** END ADD ***** 02/02/99 **************************************
     C*
     C     DDKY2         READE     PDD01                                  50
     C                   ENDDO
     C*
     C*  ALL RECOVERIES HAVE BEEN POSTED, REMOVE THE PENDING RECORD
     C*
     C*  IF PROCESSING THE WADQP ADJUSTMENT RECORD AND THE PENDING
     C*  AMOUNT IS NOT 0, DO NOT REMOVE THE RECORD.  ONLY CHANGE THE
     C*  STATUS TO BLANK.
     C*
     C                   MOVE      QQCHK#        XXCHK#
     C     DQKY          CHAIN     PDQ01                              60
     C*** BEGIN CHG ***  04/09/98  ************************************
     C*****      DQAMT     IFNE 0
     C**         DQAMTR    IFNE 0
     C*** END CHG *****  04/09/98  ************************************
     C                   MOVE      *BLANK        DQSTAT
     C  N60              UPDATE    PDQ01
     C*
     C*** BEGIN DEL ***  04/09/98  ************************************
     C*****                ELSE
     C*****                MOVE QQCHK#    XXCHK#
     C*****      DQKY      DELETPDQ01                60
     C*** END DEL *****  04/09/98  ************************************
     C**                   END
     C*
     C                   MOVE      *ON           *INLR
