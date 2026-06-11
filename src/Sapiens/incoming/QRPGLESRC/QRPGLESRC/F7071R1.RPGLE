     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F7071R1 - EOMB WORKFILE BUILD                            *
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* PROGRAMMER: C:02                                              *
     H* CREATED:    07/31/98  PROJECT: 48MBS                          *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  07/27/99  C:62  PROJECT:MC201                                *
     H*                  ADD CONDITIONS FOR AUTO CHECK ISSUANCE.      *
     H*                                                               *
     H*  04/06/99  C:94  PROJECT: SUP17545                            *
     H*                  CHANGE LOGICAL FROM FAEOL2 TO FAEOL4         *
     H*                                                               *
     H*                                                               *
     H*  02/17/99  C:94  PROJECT: 48MBV                               *
     H*                  DEL UPDATE OF EOSTAT AND EOFLG3              *
     H*                                                               *
     H*  11/10/98  C:94  PROJECT: 87MBV                               *
     H*                  ADDED ALL UPDATES THAT WERE DONE IN          *
     H*                  F6020R AND F7071R1 (BASE VERSION)            *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60 - GENERAL CHAIN INDICATOR                                  *
     H* 61 - GENERAL CHAIN INDICATOR                                  *
     H*                                                               *
     H*****************************************************************
     F*** EOMB File selected on EOSTAT *NE 'P'
     F*** BEG CHG *** 11/10/98 ***************************************
     FFAEOL4    UF   E           K DISK
     F**********FAEOL2  UF  E           K        DISK
     F*** END CHG *** 11/10/98 ***************************************
     FFYEOP     O    E           K DISK
     F                                     RENAME(PEO01:PEO02)
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     F*** BEG ADD *** 11/10/98 ***************************************
     F*
     F***** FROM BASE F7071R1 *****
     F*
     FSSYLF     IF   E           K DISK
     F*
     FFMF1L     IF   E           K DISK
     F*
     F***** FROM F6020R *****
     F*
     FFDCALL    UF   E           K DISK
     F*

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  FBAL04                 1     10
     D  XXWCPR                 2      2
      *
     D*** END ADD *** 11/10/98 ***************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*** BEG ADD *** 11/10/98 ***************************************
     C*
     C*  FS BASE CONTROL FILE KEY LIST
     C*
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
     C*
     C*  EOB FILE KEY LIST
     C*
     C     EOKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  FUND AUXILIARY FILE KEY LIST
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C     CAKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C                   KFLD                    EOEOB#
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
     C* ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQWCPR            1
     C*
     C*  INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOFND         XXFND
     C     *LIKE         DEFINE    EOAMT         XXPAY
     C     *LIKE         DEFINE    F1OUTA        XXOUTA
     C*** BEGIN ADD *** 07/27/99 **************************************
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANK        ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANK        ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANK        ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANK        ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANK        ##SDTS            1            ALLOW FUTURE SERV DT
     C*
     C*** END   ADD *** 07/27/99 **************************************
     C*
     C                   MOVE      QQCO#         XXCO#
     C*
     C                   TIME                    DSDTTM
     C*
     C* CHAIN OUT TO SYSTEM CONTROL FILE
     C*
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
     C*
     C     FBKY          CHAIN     SFB01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     XXWCPR        IFEQ      'Y'
     C                   MOVE      XXWCPR        QQWCPR
     C                   ELSE
     C                   MOVE      *BLANK        QQWCPR
     C                   ENDIF                                                  XXWCPR IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   Z-ADD     0             XXFND
     C                   Z-ADD     0             XXSUB#
     C*
     C*** END ADD *** 11/10/98 ***************************************
     C*
     C* SET UP FOR READ OF EOB FILE
     C*
     C*** BEG CHG *** 11/10/98 ***************************************
     C     EOKY          SETLL     PEO01
     C     EOKY          READE     PEO01                                  50
     C********** *LOVAL    SETLLPEO01
     C**********           READ PEO01                    50
     C*** END CHG *** 11/10/98 ***************************************
     C*
     C     *IN50         DOWEQ     *OFF
     C*
     C*** BEG ADD *** 11/10/98 ***************************************
     C*
     C     EOSUB#        IFNE      XXSUB#
     C     EOFND         ORNE      XXFND
     C*
     C                   MOVE      EOSUB#        XXSUB#
     C                   MOVE      EOFND         XXFND
     C*
     C     F1KY          CHAIN     PF101                              61
     C*
     C     *IN61         IFEQ      *OFF
     C                   MOVE      F1OUTA        XXOUTA
     C                   ENDIF                                                  *IN61 IFEQ
     C*
     C                   ENDIF                                                  EOSUB# IFNE
     C*
     C     XXOUTA        IFEQ      'Y'
     C                   MOVE      '1'           EOFLG7
     C*
     C                   ELSE
     C     EOAMT         SUB       EONDIS        XXPAY
     C*
     C     XXPAY         IFLE      0
     C                   MOVE      '2'           EOFLG7
     C                   ELSE
     C                   MOVE      '3'           EOFLG7
     C                   ENDIF                                                  XXPAY IFLE 0
     C*
     C                   ENDIF                                                  XXOUTA IFEQ
     C*
     C*
     C*** END ADD *** 11/10/98 ***************************************
     C                   CLEAR                   EOFSN
     C                   CLEAR                   EOLSN
     C                   CLEAR                   EOMID
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
      *
     C     CMSS#         CHAIN     PCD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CDFSN         EOFSN
     C                   MOVEL(P)  CDLSN         EOLSN
     C                   MOVE      CDMID         EOMID
     C                   ENDIF
     C                   ENDIF
     C*** BEG ADD *** 11/10/98 ***************************************
     C*
     C                   Z-ADD     EOEOB#        EOCHK#
     C*
     C                   MOVE      Q1PGM         EOPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*
     C                   Z-ADD     DSTIME        EOTIME
     C*
     C     CAKEY         CHAIN     PCA01                              50
     C     *IN50         DOWEQ     '0'
     C                   Z-ADD     EOEOB#        CACHK#
     C*
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C*
     C                   Z-ADD     DSTIME        CATIME
     C*
     C                   UPDATE    PCA01
     C*
     C     CAKEY         READE     PCA01                                  50
     C                   END
     C*** END ADD *** 11/10/98 ***************************************
      *
     C                   WRITE     PEO02
     C*
     C                   MOVE      EOSUB#        ##SUB#
     C                   MOVE      EOFND         ##GRP#
     C                   MOVE      EOEOB#        ##EOB#
     C                   MOVE      EOFYR         ##FYR
     C                   MOVE      EOCASE        ##CASE
     C*** BEGIN ADD *** 07/27/99 **************************************
     C     ##ACIS        IFEQ      'Y'
     C*** END   ADD *** 07/27/99 **************************************
     C*
     C                   CALL      'W0081R'
     C                   PARM      *BLANK        ##CALL            1
     C                   PARM      QQCO#         ##CO#             3
     C                   PARM                    ##SUB#            5
     C                   PARM                    ##GRP#            3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM                    ##EOB#            9
     C                   PARM      *BLANK        ##RTCD            1
     C*** BEGIN ADD *** 07/27/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 07/27/99 **************************************
      *
      *
     C*** BEG DEL *** 02/17/99 ***************************************
     C**********           MOVE 'P'       EOSTAT
     C*** END DEL *** 02/17/99 ***************************************
     C                   MOVE      'H'           EOHRFL
     C                   MOVE      'D'           EOBTFL
     C*** BEGIN UND *** 07/27/99 **************************************
     C*** BEG DEL *** 02/17/99 ***************************************
     C                   MOVE      'Y'           EOFLG3
     C*** END DEL *** 02/17/99 ***************************************
     C*** END   UND *** 07/27/99 **************************************
     C                   UPDATE    PEO01
      *
     C*** BEG CHG *** 11/10/98 ****************************************
     C     EOKY          READE     PEO01                                  50
     C**********           READ PEO01                    50
     C*** END CHG *** 11/10/98 ****************************************
      *
     C                   ENDDO                                                  *IN50 DOWEQ
      *
     C     TERM          TAG
      *
     C                   SETON                                        LR
