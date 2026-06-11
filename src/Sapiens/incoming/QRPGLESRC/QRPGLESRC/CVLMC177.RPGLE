     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 11/03/05 C:79  Project: BASECD                                *
     H*                Remove CO# from the key                        *
     H*****************************************************************
     FSMCOP     IF   E           K DISK
     FSMFCL     IF A E           K DISK
     F*

     D PRE             S              2    DIM(10) CTDATA PERRCD(1)             PREFIX, DESC
     D DSC             S             40    DIM(10) ALT(PRE)
     D FLR             S              5    DIM(10) CTDATA PERRCD(1)             BASE FLR STRG
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     FCKY          KLIST
     C*** BEG DEL *** 11/03/05 ****************************************
     C******************** KFLD           KKCO#
     C*** END DEL *** 11/03/05 ****************************************
     C                   KFLD                    KKPRE
     C*
     C     *LIKE         DEFINE    FCCO#         KKCO#
     C     *LIKE         DEFINE    FCPRE         KKPRE
     C     *LIKE         DEFINE    FCDT          XXDT
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C*** GET COMPANY #
     C*
     C                   READ      PCO01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     COCO#         KKCO#
     C                   Z-ADD     1             X
     C*
     C                   DO        10            X                 2 0
     C*
     C                   MOVEL(P)  PRE(X)        KKPRE
     C*
     C     FCKY          CHAIN     PFC01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   Z-ADD     COCO#         FCCO#
     C                   MOVEL(P)  PRE(X)        FCPRE
     C                   MOVEL(P)  DSC(X)        FCDESC
     C                   MOVEL(P)  FLR(X)        FCFLRS
     C                   MOVEL(P)  'A'           FCSTAT
     C                   MOVEL     Q1USER        FCUSER
     C                   MOVEL     Q1PGM         FCPGM
     C                   Z-ADD     XXDT          FCDT
     C                   MOVEL     DSTIME        FCTIME
     C                   MOVEL     Q1USER        FCIUSR
     C                   MOVEL     Q1PGM         FCIPGM
     C                   Z-ADD     XXDT          FCIDT
     C*
     C                   WRITE     PFC01
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   READ      PCO01                                  99
     C*
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   RETURN
** DOCUMENT PREFIX/DESCRIPTION
BLORIGINAL/REVISED/AUDIT BILL FOLDER
CCCANCELLATIONS/REINSTATEMENTS FOLDER
WCPOLICY PAGES FOLDER
RRREINSTATEMENT DOCUMENT FOLDER
CLCLAIM LETTER FOLDER
DLDEPARTMENT LETTER FOLDER
ENMIDTERM ENDORSEMENTS
MLWCMC CLAIM LETTERS
ALAGENT LETTERS
PAPAYEE LETTERS
** BASE FOLDER STRING
BILLS
CANCL
WCPOL
REINS
CLETT
DLETT
ENDOR
CLAIM
AGENT
PAYEE
