     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE F8105R - PURGE EOMB FILES
     H****************************************************************
     H*                                                              *
     H*  PROGRAM DESCRIPTION:                                        *
     H*       THIS PROGRAM WILL PURGE ALL RECORDS FOR THE FUND       *
     H*       MASTER FILES FOR THE SELECTED FUND                     *
     H*                                                              *
     H*  CALLED FROM:                                                *
     H*    F8105C - EOMB PURGE OVERRIDE CL                           *
     H*                                                              *
     H*  CALLS TO:                                                   *
     H*    NONE                                                      *
     H*                                                              *
     H*  PROGRAMMER: C:53                                            *
     H*  CREATED:    04/08/96    PROJECT: MC134                      *
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 05/20/08 C:33  Project: WC1770                                *
     H*                Ensure MC values are kept in sync with system  *
     H*                 values.                                       *
     H*                                                              *
     H*  *** NOTE ***                                                *
     H*  ANY ADDITIONAL FILES TO BE PURGED MAY BE ADDED TO W8105R1   *
     H*                                                              *
     H*  12/10/02 C:79  PROJECT: MC252                               *
     H*                 REMOVE OBSOLET FILES                         *
     H*                                                              *
     H*  08/15/00 C:62  PROJECT: MC215                               *
     H*                 REPLACE SFBPF REFERENCES WITH APIS.          *
     H*                                                              *
     H*  05/05/99  C:56   MC152                                      *
     H*                   Change FAEAL to FAEAL1, duplicate logicals *
     H*                                                              *
     H*  03/09/99 C:94 PROJECT: MC152                                *
     H*                CHANGE FDCAL2 TO FDCALL AND CHANGE KLIST      *
     H*                                                              *
     H*  02/03/98 C:61 PROJECT 60APK                                 *
     H*                Added files to purge for V3.                  *
     H*                                                              *
     H*  10/06/97 C:32 PROJECT 87MCJ                                 *
     H*                RECOMPILE DUE TO FILE CHG FOR FAE1P           *
     H*                                                              *
     H*  06/27/96 C:46 PROJECT 60AIB                                 *
     H*                ADD FILE FAERP TP PURGE PROGRAM               *
     H*                FIX-DO NOT PURGE FARPP IF A SUBSCRIBER GLOBAL *
     H*                PURGE IS DONE SINCE THIS FILE DOES NOT HAVE   *
     H*                THE SUB IN IT.                                *
     H*                                                              *
     H*  PLEASE INSERT NEW COMMENTS AT TOP OF REVISION SECTION       *
     H*                                                              *
     H****************************************************************
     H* INDICATOR USAGE:                                             *
     H*                                                              *
     H* 08 = BATCH MODE                                              *
     H* 09 = DEMAND MODE                                             *
     H* 30 = DETERMINES GLOBAL FUND DELETE                           *
     H* 60 = GENERAL CHAIN                                           *
     H*                                                              *
     H****************************************************************
     H*
      * SYSTEM FILES
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 **************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
      *
     FFACTP     UF   E           K DISK
     F*** BEG DEL *** 12/10/02 ****************************************
     F*** FADCL   UF  E           K        DISK
     F*** END DEL *** 12/10/02 ****************************************
     FFADUL     UF   E           K DISK
     FFAEOL     UF   E           K DISK
     F*** BEG DEL *** 12/10/02 ****************************************
     F*** FAE1L   UF  E           K        DISK
     F*** FAHBL   UF  E           K        DISK
     F*** END DEL *** 12/10/02 ****************************************
     FFARPL     UF   E           K DISK
     FFALTL     UF   E           K DISK
     F*** BEG CHG *** 03/09/99 ***************************************
     FFDCALL    UF   E           K DISK
     F**********FDCAL2  UF  E           K        DISK
     F*** END CHG *** 03/09/99 ***************************************
     FFDCLP     UF   E           K DISK
     FFMDTL1    UF   E           K DISK
     F*** BEG DEL *** 12/10/02 ****************************************
     F*** FDEOL   UF  E           K        DISK
     F***         PEO01                             KRENAMEPDO01
     F*** FDE1L   UF  E           K        DISK
     F***         PE101                             KRENAMEPD101
     F*** FDHCL   UF  E           K        DISK
     F*** END DEL *** 12/10/02 ****************************************
     FFZEOL     UF   E           K DISK
     F                                     RENAME(PEO01:PEOZ1)
     FFXEOL     UF   E           K DISK
     F                                     RENAME(PEO01:PEOX1)
     FTAEOL     UF   E           K DISK
     F                                     RENAME(PEO01:PEOT1)
      *** BEGIN ADD *** 06/27/96 **************************************
     FFAERL     UF   E           K DISK
      *** END ADD ***** 06/27/96 **************************************
      *
      ***** BEG ADD ******* 02/03/98 *********************************
     FFADAL     UF   E           K DISK
     F***  Begin Add  ***  05/20/09  **************************************
     F                                     INFDS(DAFDS)
     F***  End   Add  ***  05/20/09  **************************************
     F*** BEG CHG *** 05/05/99 ****************************************
     F**********FAEAL   UF  E           K        DISK
     FFAEAL1    UF   E           K DISK
     F*** END CHG *** 05/05/99 ****************************************
     FFAEDL     UF   E           K DISK
     FFAEEL     UF   E           K DISK
     FFAEHL     UF   E           K DISK
     FFAEPL     UF   E           K DISK
     FFAESL     UF   E           K DISK
     FFAEUL     UF   E           K DISK
     FFAMSL     UF   E           K DISK
     FFAPPL1    UF   E           K DISK
     FFATDL     UF   E           K DISK
     FFATIL     UF   E           K DISK
     FFATRL     UF   E           K DISK
     FFATXL     UF   E           K DISK
     FFAWHL     UF   E           K DISK
     FFDDAL     UF   E           K DISK
     F                                     RENAME(PDA01:PDA02)
     FFDDDL     UF   E           K DISK
     FFDDEL     UF   E           K DISK
     FFDDHL     UF   E           K DISK
     FFDDPL     UF   E           K DISK
     FFDDUL     UF   E           K DISK
     F                                     RENAME(PDU01:PDU02)
     FFDMGL     UF   E           K DISK
     FFMCHL1    UF   E           K DISK
     FFMCXL     UF   E           K DISK
     FFMLGL     UF   E           K DISK
      *** BEG DEL ******* 07/01/99 *********************************
     F***FXXTL2  UF  E           K        DISK
      *** END DEL ******* 07/01/99 *********************************
      *
      * zzz
      ***** END ADD ******* 02/03/98 *********************************
      *
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
      *

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D***  Begin Add  ***  05/20/09  **************************************
     D*
     D*
     D DAFDS           DS
     D  DARRN                397    400B 0
     D***  End   Add  ***  05/20/09  **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *   KEY LIST FOR ALL FILES
      *

     C     XXKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB
     C                   KFLD                    XXFND
      *
      *   KEY LIST FOR ALL FILES (GLOBAL DELETE)
      *
     C     XXKEYG        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB
      *
      *   KEY LIST FOR FILES WITHOUT SUB# IN KEY (WC)
      *
     C     XXKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWCF#
      *
      *   KEY LIST FOR FILES WITHOUT SUB# IN KEY (WC)
      *
     C     XXKYG2        KLIST
     C                   KFLD                    XXCO#
      *
      *   KEY LIST FOR THE REPORT OVERRIDE
      *
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
      *
      *   KEY LIST FOR THE COMPANY FILE
      *
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
      *
      *
      *   KEY LIST FOR THE USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *  FS BASE CONTROL FILE
      *
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** FBKY      KLIST
     C**********           KFLD           XSKEY   2
     C*** END   DEL *** 08/15/00 **************************************
      *
     C     *LIKE         DEFINE    XXFND         XXWCF#
      *
     C     *ENTRY        PLIST
     C                   PARM                    X@CO#             3
     C                   PARM                    X@SUB             5
     C                   PARM                    X@FND             3
     C                   PARM                    X@QUE            10
      *
     C     X@FND         COMP      '000'                                  30
      *
     C                   MOVE      X@CO#         XXCO#             3 0
     C                   MOVE      X@SUB         XXSUB             5 0
     C                   MOVE      X@QUE         QQQUE            10
     C  N30              MOVE      X@FND         XXFND             3 0
     C   30              Z-ADD     *ZEROS        XXFND
      *
      *   SET-UP AND INITIALIZE
      *
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
      *
      *   CLEAR ACCUMULATION FIELDS
      *
     C                   Z-ADD     *ZERO         XX#REC            6 0
     C                   Z-ADD     *ZERO         XXTREC            8 0
      *
      *   CHAIN TO THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
     C                   TIME                    DSDTTM
     C                   MOVE      X@CO#         XXCO#
     C                   MOVE      UDATE         XXDATE            6 0
      *
      *   CHAIN TO THE COMPANY FILE
      *
     C     COKEY         CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      *
      *   CONTROL INFO
      *
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           MOVE *BLANKS   XSKEY
     C**********           MOVEL'FB'      XSKEY
     C********** FBKY      CHAINSFB01                60
     C*** END   DEL *** 08/15/00 **************************************
      *
      *   PICKUP WC INFO FOR CHAIN
      *
     C     XXKEY         CHAIN     PFN01                              40
     C                   Z-ADD     FNWCF#        XXWCF#
      *
      *   GET THE DISTRIBUTION INFORMATION
      *
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      *OFF
      *
      *   PRINT THE DISTRIBUTION INFORMATION
      *
      ***** Beg Del ***** 08/26/14 *********************************************
     C********           EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   END
     C                   MOVE      *ON           *INOF
      *
     C                   EXCEPT    HEAD
      *
      *  READ/DELETE ALL RECORDS IN THE FILE FOR THE SELECTED FUND
      *
      *  FACTP - CASE TRANSFER FILE
      *
     C  N30XXKEY         CHAIN     PCT01                              60
     C   30XXKEYG        CHAIN     PCT01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCT01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PCT01                                  60                R
     C   30XXKEYG        READE     PCT01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FACTP'       XXFILE           10
     C                   EXCEPT    DET
     C*** BEG DEL *** 12/10/02 ****************************************
     C*********
     C*********
     C***FADCP*- DIAGNOSIS CODE/DETAIL CHARGE TEMP FILE
     C*********
     C**N30****  XXKEY     CHAINPDC01                60
     C***30****  XXKEYG    CHAINPDC01                60
     C*********
     C*********  *IN60     DOWEQ'0'
     C*********            DELETPDC01
     C*********            ADD  1         XX#REC
     C*********            ADD  1         XXTREC
     C**N30****  XXKEY     READEPDC01                    60            R
     C***30****  XXKEYG    READEPDC01                    60            R
     C*********            END
     C***OA****            EXCPTHEAD
     C*********            MOVE '0'       *INOA
     C*********            MOVEL'FADCP'   XXFILE 10
     C*********            EXCPTDET
     C*** END DEL *** 12/10/02 ****************************************
      *
      *  FADUP - DUP CLAIMS WORK FILE
      *
     C  N30XXKEY         CHAIN     PDU01                              60
     C   30XXKEYG        CHAIN     PDU01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDU01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PDU01                                  60                R
     C   30XXKEYG        READE     PDU01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FADUP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FAEOP EOMB/SERVICE # TRANSACTION FILE
      *
     C  N30XXKEY         CHAIN     PEO01                              60
     C   30XXKEYG        CHAIN     PEO01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEO01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PEO01                                  60                R
     C   30XXKEYG        READE     PEO01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FAEOP'       XXFILE           10
     C                   EXCEPT    DET
     C*** BEG DEL *** 12/10/02 ****************************************
     C**********
     C**********
     C***FAE1P*EOMB AUXILIARY FILE
     C**********
     C**N30***** XXKEY     CHAINPE101                60
     C***30***** XXKEYG    CHAINPE101                60
     C**********
     C********** *IN60     DOWEQ'0'
     C**********           DELETPE101
     C**********           ADD  1         XX#REC
     C**********           ADD  1         XXTREC
     C**N30***** XXKEY     READEPE101                    60            R
     C***30***** XXKEYG    READEPE101                    60            R
     C**********           END
     C***OA*****           EXCPTHEAD
     C**********           MOVE '0'       *INOA
     C**********           MOVEL'FAE1P'   XXFILE 10
     C**********           EXCPTDET
     C**********
     C***FAHBP*- HBA TRANSACTION FILE
     C**********
     C**N30***** XXKEY     CHAINPHB01                60
     C***30***** XXKEYG    CHAINPHB01                60
     C**********
     C********** *IN60     DOWEQ'0'
     C**********           DELETPHB01
     C**********           ADD  1         XX#REC
     C**********           ADD  1         XXTREC
     C**N30***** XXKEY     READEPHB01                    60            R
     C***30***** XXKEYG    READEPHB01                    60            R
     C**********           END
     C***OA*****           EXCPTHEAD
     C**********           MOVE '0'       *INOA
     C**********           MOVEL'FAHBP'   XXFILE 10
     C**********           EXCPTDET
     C*** END DEL *** 12/10/02 ****************************************
      *
      *  FALTP - REQUEST LETTER TX FILE
      *
     C  N30XXKEY         CHAIN     PLT01                              60
     C   30XXKEYG        CHAIN     PLT01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PLT01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PLT01                                  60                R
     C   30XXKEYG        READE     PLT01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FALTP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FARPP - NO MATCH ON WCMS FUND TO WCMC SUB/GROUP
      *
     C  N30XXKEY2        CHAIN     PRP01                              60
      *** BEGIN DEL *** 06/27/96 **************************************
     C***30******XXKYG2    CHAINPRP01                60
      *** END DEL ***** 06/27/96 **************************************
      *
      *** BEGIN ADD *** 06/27/96 **************************************
      *  DO NOT DELETE RECORDS FOR SUBSCRIBER GLOBAL PURGE
     C     *IN30         IFEQ      '0'
      *** END ADD ***** 06/27/96 **************************************
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRP01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY2        READE     PRP01                                  60                R
     C   30XXKYG2        READE     PRP01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FARPP'       XXFILE           10
     C                   EXCEPT    DET
      *** BEGIN ADD *** 06/27/96 **************************************
     C                   END
      *** END ADD ***** 06/27/96 **************************************
      *
      *  FDCAP - DETAIL CHARGE FILE
      *
     C  N30XXKEY         CHAIN     PCA01                              60
     C   30XXKEYG        CHAIN     PCA01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCA01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PCA01                                  60                R
     C   30XXKEYG        READE     PCA01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FDCAP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FDCLP - UNREVIEWED COLLISION TX FILE
      *
     C  N30XXKEY         CHAIN     PCL01                              60
     C   30XXKEYG        CHAIN     PCL01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCL01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PCL01                                  60                R
     C   30XXKEYG        READE     PCL01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FDCLP'       XXFILE           10
     C                   EXCEPT    DET
     C*** BEG DEL *** 12/10/02 ****************************************
     C**********
     C***FDEOP*- DELETED EOMB/SERVICE # TRANSACTION FILE
     C**********
     C**N30***** XXKEY     CHAINPDO01                60
     C***30***** XXKEYG    CHAINPDO01                60
     C**********
     C********** *IN60     DOWEQ'0'
     C**********           DELETPDO01
     C**********           ADD  1         XX#REC
     C**********           ADD  1         XXTREC
     C**N30***** XXKEY     READEPDO01                    60            R
     C***30***** XXKEYG    READEPDO01                    60            R
     C**********           END
     C***OA*****           EXCPTHEAD
     C**********           MOVE '0'       *INOA
     C**********           MOVEL'FDEOP'   XXFILE 10
     C**********           EXCPTDET
     C**********
     C***FDE1P*- DELETED EOMB AUXILIARY FILE
     C**********
     C**N30***** XXKEY     CHAINPD101                60
     C***30***** XXKEYG    CHAINPD101                60
     C**********
     C********** *IN60     DOWEQ'0'
     C**********           DELETPD101
     C**********           ADD  1         XX#REC
     C**********           ADD  1         XXTREC
     C**N30***** XXKEY     READEPD101                    60            R
     C***30***** XXKEYG    READEPD101                    60            R
     C**********           END
     C***OA*****           EXCPTHEAD
     C**********           MOVE '0'       *INOA
     C**********           MOVEL'FDE1P'   XXFILE 10
     C**********           EXCPTDET
     C**********
     C**********
     C***FDHCP*- DETAIL CHARGE AUX TX FILE
     C**********
     C**N30***** XXKEY     CHAINPHC01                60
     C***30***** XXKEYG    CHAINPHC01                60
     C**********
     C********** *IN60     DOWEQ'0'
     C**********           DELETPHC01
     C**********           ADD  1         XX#REC
     C**********           ADD  1         XXTREC
     C**N30***** XXKEY     READEPHC01                    60            R
     C***30***** XXKEYG    READEPHC01                    60            R
     C**********           END
     C***OA*****           EXCPTHEAD
     C**********           MOVE '0'       *INOA
     C**********           MOVEL'FDHCP'   XXFILE 10
     C**********           EXCPTDET
     C*** END DEL *** 12/10/02 ****************************************
      *
      *  FMDTP - DATA TRANSFER FILE
      *
     C  N30XXKEY         CHAIN     PDT01                              60
     C   30XXKEYG        CHAIN     PDT01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDT01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PDT01                                  60                R
     C   30XXKEYG        READE     PDT01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FMDTP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FZEOP - EOMB TX FILE - WORKFILE FOR EOMB BATCH REPRINT
      *
     C  N30XXKEY         CHAIN     PEOZ1                              60
     C   30XXKEYG        CHAIN     PEOZ1                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEOZ1
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PEOZ1                                  60                R
     C   30XXKEYG        READE     PEOZ1                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FZEOP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FXEOP - EOMB DAILY TX FILE
      *
     C  N30XXKEY         CHAIN     PEOX1                              60
     C   30XXKEYG        CHAIN     PEOX1                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEOX1
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PEOX1                                  60                R
     C   30XXKEYG        READE     PEOX1                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FXEOP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  TAEOP - EOMB DELETE WORK FILE
      *
     C  N30XXKEY         CHAIN     PEOT1                              60
     C   30XXKEYG        CHAIN     PEOT1                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEOT1
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PEOT1                                  60                R
     C   30XXKEYG        READE     PEOT1                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'TAEOP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *** BEGIN ADD *** 06/27/96 **************************************
      *
      *  FAERP - RECON EOMB TRANSACTION FILE
      *
     C  N30XXKEY         CHAIN     PER01                              60
     C   30XXKEYG        CHAIN     PER01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PER01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PER01                                  60                R
     C   30XXKEYG        READE     PER01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FAERP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *** END ADD ***** 06/27/96 **************************************
      *
      ***** BEG ADD ******* 02/03/98 *********************************
      *
      *  FADAP - Diagnosis code info
      *
     C  N30XXKEY         CHAIN     PDA01                              60
     C   30XXKEYG        CHAIN     PDA01                              60
      *
     C     *IN60         DOWEQ     '0'
     C***  Begin Add  ***  05/20/09  **************************************
     C                   EXSR      UPDSYS
     C***  End   Add  ***  05/20/09  **************************************
     C                   DELETE    PDA01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PDA01                                  60                R
     C   30XXKEYG        READE     PDA01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FADAP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FAEAP - EOMB Header Amount Transfer File
      *
     C  N30XXKEY         CHAIN     PEA01                              60
     C   30XXKEYG        CHAIN     PEA01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEA01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PEA01                                  60                R
     C   30XXKEYG        READE     PEA01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FAEAP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FAEDP - EOMB Dental Bill Header File
      *
     C  N30XXKEY         CHAIN     PED01                              60
     C   30XXKEYG        CHAIN     PED01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PED01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PED01                                  60                R
     C   30XXKEYG        READE     PED01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FAEDP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FAEEP - Header EOB/Notes File
      *
     C  N30XXKEY         CHAIN     PEE01                              60
     C   30XXKEYG        CHAIN     PEE01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEE01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PEE01                                  60                R
     C   30XXKEYG        READE     PEE01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FAEEP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FAEHP - EOMB Physical/HCFA Header File
      *
     C  N30XXKEY         CHAIN     PEH01                              60
     C   30XXKEYG        CHAIN     PEH01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEH01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PEH01                                  60                R
     C   30XXKEYG        READE     PEH01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FAEHP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FAEPP - EOMB Rx Bill Header File
      *
     C  N30XXKEY         CHAIN     PEP01                              60
     C   30XXKEYG        CHAIN     PEP01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEP01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PEP01                                  60                R
     C   30XXKEYG        READE     PEP01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FAEPP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FAESP - EOMB Status Transfer File
      *
     C  N30XXKEY         CHAIN     PES01                              60
     C   30XXKEYG        CHAIN     PES01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PES01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PES01                                  60                R
     C   30XXKEYG        READE     PES01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FAESP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FAMSP - Medical Service Transfer File
      *
     C  N30XXKEY         CHAIN     PMS01                              60
     C   30XXKEYG        CHAIN     PMS01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PMS01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PMS01                                  60                R
     C   30XXKEYG        READE     PMS01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FAMSP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FAEUP - EOMB Hosp/Ub Header File
      *
     C  N30XXKEY         CHAIN     PEU01                              60
     C   30XXKEYG        CHAIN     PEU01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEU01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PEU01                                  60                R
     C   30XXKEYG        READE     PEU01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FAEUP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FAPPP - Phys/Prov/Spec Transfer File
      *
     C  N30XXKEY         CHAIN     PPP01                              60
     C   30XXKEYG        CHAIN     PPP01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PPP01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PPP01                                  60                R
     C   30XXKEYG        READE     PPP01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FAPPP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FATDP - BWC Treatment Plan Notes
      *
     C  N30XXKEY         CHAIN     PTD01                              60
     C   30XXKEYG        CHAIN     PTD01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PTD01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PTD01                                  60                R
     C   30XXKEYG        READE     PTD01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FATDP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FATIP - BWC Treatment Plan Notes
      *
     C  N30XXKEY         CHAIN     PTI01                              60
     C   30XXKEYG        CHAIN     PTI01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PTI01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PTI01                                  60                R
     C   30XXKEYG        READE     PTI01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FATIP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FATRP - BWC Treatment Plan File
      *
     C  N30XXKEY         CHAIN     PTR01                              60
     C   30XXKEYG        CHAIN     PTR01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PTR01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PTR01                                  60                R
     C   30XXKEYG        READE     PTR01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FATRP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FATXP - Treatment Report Transfer File
      *
     C  N30XXKEY         CHAIN     PTX01                              60
     C   30XXKEYG        CHAIN     PTX01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PTX01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PTX01                                  60                R
     C   30XXKEYG        READE     PTX01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FATXP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FAWHP - Work History File
      *
     C  N30XXKEY         CHAIN     PWH01                              60
     C   30XXKEYG        CHAIN     PWH01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PWH01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PWH01                                  60                R
     C   30XXKEYG        READE     PWH01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FAWHP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FDDAP - EOMB Detail Amount Transfer File
      *
     C  N30XXKEY         CHAIN     PDA02                              60
     C   30XXKEYG        CHAIN     PDA02                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDA02
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PDA02                                  60                R
     C   30XXKEYG        READE     PDA02                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FDDAP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FDDDP - EOMB Detail Dental File
      *
     C  N30XXKEY         CHAIN     PDD01                              60
     C   30XXKEYG        CHAIN     PDD01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDD01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PDD01                                  60                R
     C   30XXKEYG        READE     PDD01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FDDDP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FDDEP - EOMB Detail EOB/Notes File
      *
     C  N30XXKEY         CHAIN     PDE01                              60
     C   30XXKEYG        CHAIN     PDE01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDE01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PDE01                                  60                R
     C   30XXKEYG        READE     PDE01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FDDEP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FDDHP - EOMB Phys/HCFA Detail File
      *
     C  N30XXKEY         CHAIN     PDH01                              60
     C   30XXKEYG        CHAIN     PDH01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDH01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PDH01                                  60                R
     C   30XXKEYG        READE     PDH01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FDDHP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FDDPP - EOMB Pharmacy Detail File
      *
     C  N30XXKEY         CHAIN     PDP01                              60
     C   30XXKEYG        CHAIN     PDP01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDP01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PDP01                                  60                R
     C   30XXKEYG        READE     PDP01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FDDPP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FDDUP - EOMB Hosp/Ub Detail File
      *
     C  N30XXKEY         CHAIN     PDU02                              60
     C   30XXKEYG        CHAIN     PDU02                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDU02
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PDU02                                  60                R
     C   30XXKEYG        READE     PDU02                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FDDUP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FDMGP - Bill Entry Message File
      *
     C  N30XXKEY         CHAIN     PMG01                              60
     C   30XXKEYG        CHAIN     PMG01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PMG01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PMG01                                  60                R
     C   30XXKEYG        READE     PMG01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FDMGP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FMCHP - Claim Status History File
      *
     C  N30XXKEY         CHAIN     PCH01                              60
     C   30XXKEYG        CHAIN     PCH01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCH01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PCH01                                  60                R
     C   30XXKEYG        READE     PCH01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FMCHP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FMCXP - WCMC Control File
      *
     C  N30XXKEY         CHAIN     PCX01                              60
     C   30XXKEYG        CHAIN     PCX01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCX01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PCX01                                  60                R
     C   30XXKEYG        READE     PCX01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FMCXP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FMLGP - EDI Record
      *
     C  N30XXKEY         CHAIN     PLG01                              60
     C   30XXKEYG        CHAIN     PLG01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PLG01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PLG01                                  60                R
     C   30XXKEYG        READE     PLG01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FMLGP'       XXFILE           10
     C                   EXCEPT    DET
     C*** BEG DEL ******* 07/01/99 *********************************
     C***
     C***FXXTP - EDI Extract Workfile
     C***
     C**N30      XXKEY     CHAINPXT01                60
     C***30      XXKEYG    CHAINPXT01                60
     C***
     C***        *IN60     DOWEQ'0'
     C***                  DELETPXT01
     C***                  ADD  1         XX#REC
     C***                  ADD  1         XXTREC
     C**N30      XXKEY     READEPXT01                    60            R
     C***30      XXKEYG    READEPXT01                    60            R
     C***                  END
     C***OA                EXCPTHEAD
     C***                  MOVE '0'       *INOA
     C***                  MOVEL'FXXTP'   XXFILE 10
     C***                  EXCPTDET
     C*** END DEL ******* 07/01/99 *********************************
      *
      * zzz
      ***** END ADD ******* 02/03/98 *********************************
      *
      *
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    TOTL
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
      *
     C***  Begin Add  ***  05/20/09  **************************************
     C*====================================================================
     C* Update the system values
     C*====================================================================
     C     UPDSYS        BEGSR
     C*
     C                   CALL      'FA275R'
     C                   PARM      DARRN         ##RRN             9 0
     C                   PARM      'D'           ##MODE            1
     C                   PARM      'MC'          ##FROM            2
     C                   PARM      'SY'          ##TO              2
     C*
     C                   ENDSR
     C***  End   Add  ***  05/20/09  **************************************
      *
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE          2
     O                                           23 'REPORT SELECTION:'
     O          E            ROUTE          2
     O                                           20 'CO#:'
     O                       XXCO#               25
     O          E            ROUTE          2
     O                                           20 'DATE:'
     O                       XXDATE              30 '  /  /  '
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                                51 '.1996'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      ****************************************************************
      *     HEADING
      ****************************************************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              84
     O                       XXDATE        Y    130
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              94
     O                                          124 'PAGE:'
     O                       PAGE          Z    130
     O          E            HEAD           2
     O                                           18 'FILE NAME'
     O                                           44 '# DELETED RECORDS'
      ****************************************************************
      *     DETAIL LINE WITH FILE NAME AND COUNT
      ****************************************************************
     O          E            DET            1
     O                       XXFILE              19
     O                       XX#REC        JB    40
      ****************************************************************
      *     TOTAL RECORD COUNT FOR ALL FILES
      ****************************************************************
     O          E            TOTL        2  2
     O                                           18 'ALL FILES'
     O                       XXTREC        J     40
