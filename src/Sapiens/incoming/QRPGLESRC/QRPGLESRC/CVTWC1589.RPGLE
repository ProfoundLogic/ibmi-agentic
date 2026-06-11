     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H* LOAD MTC BY STATE TABLE VALUES.
     H**
     H* CREATED: 02/13/08 BY C:190
     H**
     H*****************************************************************
     H*
     FSMCOP     IF   E           K DISK
     FWTGIL     IF A E           K DISK
     FWTGTL     IF A E           K DISK

     D*
     D TKY             S              2    DIM(6) CTDATA PERRCD(1)
     D TDS             S             40    DIM(6) ALT(TKY)
     D SKY             S              2    DIM(25) CTDATA PERRCD(1)
     D SDS             S             40    DIM(25) ALT(SKY)
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D TDSC            C                   CONST('FL FROI MTC CODES')
     D SDSC            C                   CONST('FL SROI MTC CODES')

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     GIKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    GTDESC        KKDESC
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C*** GET COMPANY #
     C                   READ      PCO01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     COCO#         KKCO#             3 0
     C*
     C                   MOVEL     'FLE1'        KKTBID
     C                   MOVEL(P)  TDSC          KKDESC
     C                   EXSR      WRTGIP
     C     1             DO        6             X                 2 0
     C*
     C                   MOVEL(P)  TKY(X)        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   Z-ADD     KKCO#         GTCO#
     C                   MOVEL(P)  KKTBID        GTTBID
     C                   MOVEL(P)  KKKEY         GTKEY
     C                   MOVEL(P)  TDS(X)        GTDESC
     C*
     C                   MOVEL(P)  'A'           GTSTAT
     C                   MOVEL     Q1USER        GTUSER
     C                   MOVEL     Q1PGM         GTPGM
     C                   MOVEL     XXDT          GTDT
     C                   MOVEL     DSTIME        GTTIME
     C*
     C                   WRITE     PGT01
     C*
     C                   ENDIF
     C                   ENDDO
     C*
     C                   MOVEL     'FLE2'        KKTBID
     C                   MOVEL(P)  SDSC          KKDESC
     C                   EXSR      WRTGIP
     C     1             DO        25            X                 2 0
     C*
     C                   MOVEL(P)  SKY(X)        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   Z-ADD     KKCO#         GTCO#
     C                   MOVEL(P)  KKTBID        GTTBID
     C                   MOVEL(P)  KKKEY         GTKEY
     C                   MOVEL(P)  SDS(X)        GTDESC
     C*
     C                   MOVEL(P)  'A'           GTSTAT
     C                   MOVEL     Q1USER        GTUSER
     C                   MOVEL     Q1PGM         GTPGM
     C                   MOVEL     XXDT          GTDT
     C                   MOVEL     DSTIME        GTTIME
     C*
     C                   WRITE     PGT01
     C*
     C                   ENDIF
     C                   ENDDO
     C                   READ      PCO01                                  99
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
     C*****************************************************************
     C* WRTGIP - WRITE FTGIP
     C*****************************************************************
     C     WRTGIP        BEGSR
     C*
     C     GIKY          CHAIN     PGI01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   Z-ADD     KKCO#         GICO#
     C                   MOVEL(P)  KKTBID        GITBID
     C                   MOVEL(P)  KKDESC        GIDESC
     C                   MOVEL(P)  'MTC CODE'    GIFN01
     C*
     C                   MOVEL(P)  'A'           GISTAT
     C                   MOVEL     Q1USER        GIUSER
     C                   MOVEL     Q1PGM         GIPGM
     C                   MOVEL     XXDT          GIDT
     C                   MOVEL     DSTIME        GITIME
     C*
     C                   WRITE     PGI01
     C                   ENDIF
     C*
     C                   ENDSR
** TKY  - TDS                              |
00FIRST REPORT OF INJURY
01CANCEL TRANSACTION
02CHANGE TRANSACTION
04DENIAL TRANSACTION
AQACQUIRED CLAIM
AUACQUIRED/UNALLOCATED
** SKY  - SDS
02CHANGE
04DENIAL
ABADD CONCURRENT BENEFIT TYPE
APACQUIRED/PAYMENT
CACHANGE IN BENEFIT AMOUNT
CBCHANGE IN BENEFIT TYPE
CDCOMPENSABLE DEATH NO DEPENDENTS/PAYEES
EPEMPLOYER PAID
EREMPLOYER REINSTATEMENT
FNFINAL PAYMENT
IPINITIAL PAYMENT
P7PARTIAL SUSPENSION, BENEFITS EXHAUSTED
PDPARTIAL DENIAL
PYPAYMENT REPORT
RBREINSTATEMENT OF BENEFIT
S1SUSPENSION, RTW OR MED QUALIFIED TO RTW
S2MEDICAL NON-COMPLIANCE
S3ADMINISTRATIVE NON-COMPLIANCE
S4CLAIMANT DEATH
S5INCARCERATION
S6CLAIMANT'S WHEREABOUTS UNKNOWN
S7BENEFITS EXHAUSTED
S8JURISDICTION CHANGE
VEVOLUNTEER
SASUB-ANNUAL
