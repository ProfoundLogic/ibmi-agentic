     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE CVTWMBIP60
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION                                          *
     H*                                                               *
     H*    Update the following fields of Bank Information            *
     H*    File (WMBIP):                                              *
     H*                                                               *
     H*         BIACCT           BANK ACCT #                          *
     H*         BIEFDT           EFF DATE                             *
     H*         BIAPPL           APPLIC'N CODE                        *
     H*         BISTAT           STATUS                               *
     H*         BINAM1           SIGNATURE #1                         *
     H*         BICMD1           PRT CMD STR #1                       *
     H*         BIMACR           MACRO ID                             *
     H*         BIFTYP           FORMTYPE                             *
     H*         BIHDCY           HRDCPY PROD ?                        *
     H*         BIOUTQ           CHECK OUTQ                           *
     H*         BICPYQ           FILE CPY OUTQ                        *
     H*         BIANTH           ANTHEM Y/N ?                         *
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H*  CREATED:                                                     *
     H*                                                               *
     H*  02/13/97 C:61 PROJECT 87WBL                                  *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*      60    READ/CHAIN Bank Info File WMBIL                    *
     H*      61    READ/CHAIN Fund Auxiliary WDF2L                    *
     H*      62    READ/CHAIN Signature Master WMSGL                  *
     H*      63    READ/CHAIN Fund Master WMFNL                       *
     H*                                                               *
     H*****************************************************************
      *
      *** Bank Information File
     FWMBIL     UF   E           K DISK
      *
      *** Check Signature Master
     FWMSGL     IF   E           K DISK
      *
      *** "Old" Fund Master
     FWMFNL     IF   E           K DISK
      *
      *** "Old" Bank Accout master
     FWMBAL     IF   E           K DISK
      *
      *
      * SYSTEM DATE AND TIME

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *  "Old" Fund Master Key
      *

     C     FNKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  "Old" Bank Account Master
      *
     C     BAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  Check Signature Master Key
      *
     C     SGKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXID              3 0
      *
     C     *LIKE         DEFINE    BICO#         XXCO#
     C     *LIKE         DEFINE    BIFND         XXFND
      ****************************************************************
      *  MAIN LOOP
      ****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQEFDT            7            CYYMMDD
     C                   PARM                    QQCHKQ           10            Check OUTQ
     C                   PARM                    QQCPYQ           10            File Copy OUTQ
     C*
      *
      *  INITIALIZE
      *
     C                   MOVE      QQEFDT        XXEFDT            7 0
      *
      *  PROCESS BANK INFORMATION FILE RECORDS
      *
     C                   READ      PBI01                                  60
      *
     C     *IN60         DOWEQ     '0'
     C     BISTAT        ANDNE     'I'                                          ONLY ACTIVE
      *
      *                    *** Build Key CO#/FND
     C                   Z-ADD     BICO#         XXCO#
     C                   Z-ADD     BIFND         XXFND
      *
     C     BAKY1         CHAIN     PBA01                              61
      *
     C     *IN61         IFEQ      *ON
      *
     C                   DELETE    PBI01
      *
     C                   ELSE
      *
     C                   MOVEL     BAACCT        BIACCT                         BANK ACCT #
     C                   Z-ADD     XXEFDT        BIEFDT                         EFF DATE
     C                   MOVE      'WC'          BIAPPL                         APPLIC'N CODE
     C                   MOVE      'A'           BISTAT                         STATUS
      *
      *  SIG 1 SUBSCRIPT
      *
     C     BI50N1        IFGT      *ZERO
      *
      *                    *** Build Key CO#/ID
     C                   MOVE      *ZERO         XXID
     C                   MOVE      BI50N1        XXID
     C                   ADD       100           XXID
      *
     C     SGKY1         CHAIN     PSG01                              62
     C     *IN62         IFEQ      '0'
      *
     C                   MOVEL     SGNAME        BINAM1                         SIGNATURE #1
     C                   MOVEL     SGCMD         BICMD1                         PRT CMD STR #1
      *
     C                   ENDIF                                                  SIGNAT 1 FOUND
     C                   ENDIF                                                  BI50N1 GT ZERO
      *
     C     BI50N2        IFGT      *ZERO
      *
      *                    *** Build Key CO#/ID
     C                   MOVE      *ZERO         XXID
     C                   MOVE      BI50N2        XXID
     C                   ADD       100           XXID
      *
     C     SGKY1         CHAIN     PSG01                              62
      *
     C     *IN62         IFEQ      '0'
      *
     C                   MOVEL     SGNAME        BINAM2                         SIGNATURE #2
     C                   MOVEL     SGCMD         BICMD2                         PRT CMD STR #2
      *
     C                   ENDIF                                                  SIGNAT 2 FOUND
     C                   ENDIF                                                  BI50N2 GT ZERO
      *
     C                   MOVE      XXFND         BIMACR                         MACRO ID
     C                   MOVEL     'Y'           BIHDCY                         HRDCPY PROD ?
     C                   MOVEL     QQCHKQ        BIOUTQ                         CHECK OUTQ
     C                   MOVEL     QQCPYQ        BICPYQ                         FILE CPY OUTQ
     C                   MOVEL     'N'           BIANTH                         ANTHEM Y/N ?
      *
     C     FNKY1         CHAIN     PFN01                              63
      *
     C     *IN63         IFEQ      '0'
     C** ********          MOVELFNCHTP    BICKTP           CHECK TYPE
     C                   MOVE      *BLANK        BICKTP                         CHECK TYPE
     C                   MOVEL(P)  FNNAME        BI40A1
     C                   MOVEL(P)  FNMAD1        BI40A2
      *
     C     FNMAD2        IFNE      *BLANK
     C                   MOVEL     FNMAD2        BI40A3
      *
     C     FNMCTY        CAT(P)    ',':0         BI40A4
     C                   CAT       FNMST:1       BI40A4
     C                   MOVEL(P)  FNMZIP        XA5               5
     C                   MOVE(P)   FNMZIP        XA4               4
     C                   CAT       XA5:2         BI40A4
     C                   CAT       '-':0         BI40A4
     C                   CAT       XA4:0         BI40A4
     C                   ENDIF
      *
     C     FNMAD2        IFEQ      *BLANK
     C     FNMCTY        CAT(P)    ',':0         BI40A3
     C                   CAT       FNMST:1       BI40A3
     C                   MOVEL(P)  FNMZIP        XA5               5
     C                   MOVE(P)   FNMZIP        XA4               4
     C                   CAT       XA5:2         BI40A3
     C                   CAT       '-':0         BI40A3
     C                   CAT       XA4:0         BI40A3
      *
     C                   MOVE      *BLANK        BI40A4
     C                   ENDIF
     C                   ENDIF                                                  HAV FND MST REC
      *
     C                   MOVE      XXFND         CFND              3
     C     BICKTP        CAT       CFND:0        BIFTYP                         FORMTYPE
      *
     C                   MOVEL(P)  Q1USER        BIUSER
     C                   MOVEL(P)  Q1PGM         BIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     Q1TIME        BITIME
     C                   Z-ADD     Q1DT          BIDT
     C                   UPDATE    PBI01
      *
     C                   ENDIF                                                  HAV FND AUX REC
      *
     C                   READ      PBI01                                  60
      *
     C                   ENDDO                                                  HAVE BI REC
      *
      * END PROGRAM
      *
     C                   SETON                                        LR
      *
