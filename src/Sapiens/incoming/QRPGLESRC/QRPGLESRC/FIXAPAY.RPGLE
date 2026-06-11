     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE Create WDPAP/WDRPP from WARPPM record
     H*****************************************************************
     H* Description:  This pgm will create a single payment and detail
     H*   record from WARPPM based on RRN#
     H*
     H*****************************************************************
     H*  CREATED:
     H*
     H*  01/23/19 C:053 SUPPORT: 75327
     H*****************************************************************
     FWARPPM    IF   E             DISK
     FWDPAP     O    E             DISK
     FWDPAL     IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
     FWDRPP     O    E             DISK
     F                                     RENAME(PRP01:PRP02)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     C     *ENTRY        PLIST
     C                   PARM                    ##RRN             9
     C*
     C     PAKEY1        KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   Z-ADD     1160510       XXDT              7 0
     C                   MOVE      ##RRN         QQRRN             9 0
     C*
     C     QQRRN         CHAIN     PRP01                              99
     C     *IN98         CABEQ     *ON           END
     C*
     C     PAKEY1        SETGT     PPA02
     C     PAKEY1        READPE    PPA02                                  98
     C     *IN98         IFEQ      *ON
     C                   Z-ADD     1             XXSEQ#            5 0
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
     C                   MOVEL     'FIXAPAY'     PAPGM
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADT
     C                   MOVE      *ZEROS        PARQDT
     C                   Z-ADD     DSTIME        PATIME
     C                   Z-ADD     XXDT          PATRDT
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
     C                   MOVE      *BLANK        PASTAT                         Recovery Adjustmnt f
     C                   MOVEL     RPINV#        PAINV#
     C                   MOVEL     'PDSPGMR'     PAIUSR
     C                   MOVEL     'FIXAPAY'     PAIPGM
     C                   MOVEL     SRDTO         PAIDT
     C                   Z-ADD     DSTIME        PAITIM
     C*
     C                   WRITE     PPA01
     C*
     C                   MOVE      XXDT          RPTRDT
     C                   Z-ADD     PASEQ#        RPSEQ#
     C                   Z-ADD     DSTIME        RPTIME
     C                   WRITE     PRP02
     C*
     C     END           TAG
     C                   SETON                                        LR
