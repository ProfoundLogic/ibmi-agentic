     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F8084R - HES Reconciliation File Program
     H*
     H*****************************************************************
     H*                                                               *
     H*  Called from: W8084C - HES Recon File - Override Pgm          *
     H*  Calls to   : None                                            *
     H*                                                               *
     H*****************************************************************
     H*  CREATED: 11/03/09    PROGRAMMER: 227     PROJECT: 135MMX     *
     H*                                        Copied from F8263R     *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*  08/08/05  C: 79  Proj: 135MET                                *
     H*                   Select only Concentra Generated Checks      *
     H*                                                               *
     H*****************************************************************
     FFAH1L5    UF   E           K DISK
     FWARPPML   IF   E           K DISK
     FFAEAL1    IF   E           K DISK
     FFAHRP     O    E             DISK
     F*
      /COPY SCOPYWW01D
     D*
     D***PY SCOPYWW01I
     D*****************************************************************
     D* Program constants.
     D*****************************************************************
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@DCID           12
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDYY'       SRFFMT            8
     C                   PARM      '*MDY'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     RPKY          KLIST
     C                   KFLD                    H1CO#
     C                   KFLD                    H1FND
     C                   KFLD                    H1FYR
     C                   KFLD                    H1CASE
     C                   KFLD                    H1EOB#
     C*
     C     EAKY          KLIST
     C                   KFLD                    H1CO#
     C                   KFLD                    H1SUB#
     C                   KFLD                    H1FND
     C                   KFLD                    H1FYR
     C                   KFLD                    H1CASE
     C                   KFLD                    H1EOB#
     C*================================================================
     C*  LIKE defines
     C*================================================================
     C*
     C* EXECUTE MAIN SUBROUTINE
     C*
     C                   EXSR      MAINSR
     C*
     C*================================================================
     C* MAINSR - MAIN SUBROUTINE
     C*================================================================
     C     MAINSR        BEGSR
     C*
     C                   TIME                    DSDTTM
     C                   READ      PH101                                  90
     C     *IN90         DOWEQ     *OFF
     C     EAKY          CHAIN     PEA01                              60
     C     *IN60         IFEQ      *OFF
     C     H1PRFL        IFEQ      'D'
     C     EAWCPD        ORGT      0
     C     EAEPFL        OREQ      'P'
     C*
     C     EAEPFL        IFEQ      'P'
     C     RPKY          CHAIN     PRP01                              61
     C     RPSTAT        IFEQ      'D'
     C     EAPAID        OREQ      0
     C                   EXSR      WRITER
     C*** BEG ADD *** 02/17/10 ****************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         H1RCDT
     C                   UPDATE    PH101
     C*** END ADD *** 02/17/10 ****************************************
     C                   READ      PH101                                  90
     C                   ITER
     C                   ENDIF                                                  RPSTAT='D'
     C                   ENDIF                                                  EAEPFL='P'
     C*
     C                   EXSR      WRITER
     C*** BEG ADD *** 02/17/10 ****************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         H1RCDT
     C                   UPDATE    PH101
     C*** END ADD *** 02/17/10 ****************************************
     C*
     C                   ENDIF                                                  H1PRFL='D'
     C                   ENDIF                                                  *IN60=*OFF
     C                   READ      PH101                                  90
     C*
     C                   ENDDO
     C*
     C                   EXSR      PGMEND
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of Program Processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVEL     *BLANKS       @@DCID
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         DATE              6
     C                   MOVEL     DSTIME        DSTM              6
     C     'HR'          CAT(P)    DATE:0        @@DCID
     C                   CAT(P)    '.TXT':0      @@DCID
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write the Record in FAHRP
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   PHR01
     C                   MOVEL(P)  H1CR#         HRCR#
     C                   MOVE      *ALL'0'       HRCLM#
     C                   MOVE      H1CLM#        HRCLM#
     C                   MOVE      EAWCPD        HRPDT
     C                   MOVE      EAPAID        HRPAID
     C*
     C                   WRITE     PHR01
     C*
     C                   ENDSR
