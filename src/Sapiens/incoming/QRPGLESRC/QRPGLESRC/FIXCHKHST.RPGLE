     H* (c) 2018 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H/TITLE FIXCHKHST - Fix checks due to invalid COBUL1
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H* This program is run if COBUL1 or RPBUL1 is corrupted causing
     H* the detail records for WDPAP & WDRPP not get written.
     H* Claim and Policy information should have been updated and the checks
     H* would have been printed.
     H*
     H* NOTE:
     H* COBUL1 must be set to an appropriate value
     H*   May be set to a greater value than last valid BUL1 + affected
     H*   record count + some buffer
     H*
     H* SETUP:
     H* Copy affected records from WARPPM to WARPP in an isolated library
     H*   Change RPSTAT to blank in iso lib
     H*   Clear RPBUL1 and set to desired BUL1 value
     H*     These values must be ones that COBUL1 will not use in the future
     H* Copy WARPLN & LS over WARPP in iso lib
     H* Copy WACPP & L4 over to iso lib (affected run records only)
     H*   Change CPSTA1 to blank in iso lib
     H*
     H* IF GROUP CHECKS ARE AFFECTED:
     H* Copy WACPP & L4 over to iso lib (affected run records only)
     H*   Change CPSTA1 to blank in iso lib
     H*
     H* TESTING:
     H* OK to have WDPAP/L/L1 & WDRPP/L1 in iso lib
     H*
     H* LIVE RUN:
     H* WDPAP/L/L1 & WDRPP/L1 should be removed from iso lib
     H*   Next Sequence # is used from WDPAP so need the live file
     H*
     H* This program should be called from FIXCHKHSTC with the isolated lib
     H* before the clients live libary list
     H*   PARMS                RUNDATE    CO#  CHK TYPE
     H*   Regular checks      'mmddccyy' '999' ' '
     H*   Group checks        'mmddccyy' '999' 'G'
     H*
     H*****************************************************************
     H* CREATED:
     H*
     H* 09/12/18 C:53  SUPPORT: 73297
     H*
     H*****************************************************************
     FWARPLS    UF   E           K DISK
     FWDPAL1    IF A E           K DISK
     FWDPAL     IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
     FWACPL4    UF   E           K DISK
     FWDRPL1    O    E           K DISK
     F                                     RENAME(PRP01:PRP02)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     IPRP01
     I              RPPGM                       R1PGM
     I              RPDT                        R1DT
     I              RPTIME                      R1TIME
     I              RPTRDT                      R1TRDT
     I              RPSEQ#                      R1SEQ#
     C*
     C     *LIKE         DEFINE    RPDT          XXDT
     C     *LIKE         DEFINE    RPCO#         ##CO#
     C     *LIKE         DEFINE    RPFND         ##FND
     C     *LIKE         DEFINE    RPFYR         ##FYR
     C     *LIKE         DEFINE    RPCASE        ##CASE
     C     *LIKE         DEFINE    RPEOB#        ##EOB#
     C     *LIKE         DEFINE    RPCHK#        ##CHK#
     C     *LIKE         DEFINE    RPDT          ##CHDT
     C     *LIKE         DEFINE    RPAMT         ##CHAT
     C     *LIKE         DEFINE    RPEOBS        ##RCN
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDATE            8
     C                   PARM                    QQCO#             3
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
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C                   MOVE      QQDATE        XXDATE            8 0
     C                   MOVE      QQCO#         XXCO#             3 0
     C*
     C*  REQUEST PAYMENT KEY LIST
     C*
     C     RPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CPCKTP
     C                   KFLD                    CPFND
     C*
     C*  PAYMENT DETAIL KEY LIST
     C*
     C     PAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPBUL1
     C*
     C*
     C     PAKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C*
     C*  CHECK PROCESSING FILE
     C*
     C     CPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C*
     C*  REQUEST PAYMENT DETAIL FILE
     C*
     C     RPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPBUL1
     C*
     C*  START READING THE CHECK PROCESSING FILE, WE MUST HAVE
     C*  A RECORD FOR THIS DATE
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C     CPKEY         CHAIN     PCP01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  Only process groups if checks were printed.
     C*
     C     CPSTA0        IFEQ      'Y'
     C*
     C*  THE CHECK PROCESSING FLAG MUST BE BLANK
     C*
     C     CPSTA1        IFEQ      ' '
     C*
     C*  START READING ALL PAYMENT RECORDS WHICH HAVE NOT BEEN UPDATED
     C*
     C     RPKEY         CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C     R1TRDT        IFLE      XXDT
     C     RPBUL1        ANDNE     0
     C*
     C*  WE NEED TO WRITE THE PAYMENT DETAIL RECORD FROM THE
     C*  TRANSACTION
     C*  WE ARE GOING WITH THE LOG NUMBER AS KEY
     C*
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      '1'
     C*
     C*  WE NEED THE LAST SEQUENCE NUMBER FROM THE FILE FOR THIS CLAIM
     C*
     C     PAKEY1        SETGT     PPA02
     C                   READP     PPA02                                  60
     C     PACO#         IFNE      RPCO#
     C     PAFND         ORNE      RPFND
     C     PAFYR         ORNE      RPFYR
     C     PACASE        ORNE      RPCASE
     C                   Z-ADD     1             XXSEQ#            5 0
     C*
     C                   ELSE
     C     PASEQ#        ADD       1             XXSEQ#
     C                   END
     C*
     C*  MOVE THE TRANSACTION RECORD TO THE PAYMENT DETAIL RECORD
     C*
     C                   MOVE      RPCO#         PACO#
     C                   MOVE      RPFND         PAFND
     C                   MOVE      RPFYR         PAFYR
     C                   MOVE      RPCASE        PACASE
     C                   MOVE      XXSEQ#        PASEQ#
     C                   MOVE      RPCHK#        PACHK#
     C                   MOVE      *ZERO         PABASQ
     C                   MOVE      XXDT          PATRDT
     C                   MOVE      XXDT          PAPODT
     C                   MOVE      RPFEIN        PAFEIN
     C                   MOVE      RPMOD         PAMOD
     C                   MOVE      RPAMT         PAAMT
     C                   MOVE      RPBR2         PABR2
     C                   MOVE      RPBR7         PABR7
     C                   MOVE      RPFRDT        PAFRDT
     C                   MOVE      RPTODT        PATODT
     C                   MOVE      RPWKS         PAWKS
     C                   MOVE      RPDAYS        PADAYS
     C                   MOVE      RPTRCD        PATRCD
     C                   MOVE      RPTYPE        PATYPE
     C                   MOVE      RPRECD        PARECD
     C                   MOVE      RPRETP        PARETP
     C                   MOVE      RPDESC        PADESC
     C                   MOVE      *BLANK        PAADST
     C                   MOVE      RPBUL1        PABUL1
     C                   MOVE      *BLANK        PASTAT
     C                   MOVE      RPUSER        PAUSER
     C                   MOVE      Q1PGM         PAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADT
      *
      * Retrieve the Client ID
     C                   MOVEL     SRDTO         ZZSYDT            7 0
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C                   MOVE      *ZEROS        PARQDT
     C     CLTID         IFEQ      '139'
     C                   MOVE      R1TRDT        PARQDT
     C                   MOVEL     SRDTO         PATRDT
     C                   ENDIF
     C*
     C                   Z-ADD     DSTIME        PATIME
     C                   MOVE      RPPGTP        PAPGTP
     C                   Z-ADD     RPEOB#        PAEOB#
      *
     C                   MOVEL     RPCKTP        PACKTP                         Bank Acct# Code
      /Free
       PAeftc = RPeftc;
      /End-Free
     C                   MOVEL     RPACCT        PAACCT                         Bank Account
     C                   MOVEL     RPOFF#        PAOFF#                         Office #
     C                   Z-ADD     RPRCVD        PARCVD                         Date Received
     C                   Z-ADD     RPDUE         PADUE                          Due Date
     C                   MOVEL     RPLTCD        PALTCD                         PMT Late Code
     C                   Z-ADD     *ZERO         PADPCD                         Dup PMT Clear Date
     C                   MOVE      *BLANK        PADPCU                         Dup PMT Clear User
     C                   MOVEL     RPRQST        PARQST                         Initial Requester
     C                   MOVEL     RPFNAL        PAFNAL                         Final Comp Paymnt fl
     C                   Z-ADD     *ZERO         PARCDT                         Reconciliation Date
     C                   MOVE      *BLANK        PARADJ                         Recovery Adjustmnt f
     C                   MOVE      RPEOBS        PAEOBS
     C                   Z-ADD     RPDSQ#        PADSQ#
      *
     C                   MOVEL     RPINV#        PAINV#
      *
     C                   MOVE      Q1USER        PAIUSR
     C                   MOVE      Q1PGM         PAIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAIDT
     C                   Z-ADD     DSTIME        PAITIM
     C                   WRITE     PPA01
     C*
     C*  Load the View Check detail file (WDRPP)
     C*
     C                   MOVE      Q1PGM         RPPGM
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      XXDT          RPDT
     C                   MOVE      XXDT          RPTRDT
     C                   Z-ADD     PASEQ#        RPSEQ#
     C*
     C                   Z-ADD     DSTIME        RPTIME
     C                   WRITE     PRP02
     C*
     C                   END
     C                   END
     C     RPKEY         READE     PRP01                                  60
     C                   END
     C*
     C                   END
     C*
     C*  UPDATE THIS RECORD AS PROCESSED
     C*
     C********           MOVE      'Y'           CPSTA1
     C*********          UPDATE    PCP01
     C                   ENDIF
     C*
     C     CPKEY         READE     PCP01                                  60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
