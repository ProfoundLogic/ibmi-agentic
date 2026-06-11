     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR**dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE FA737R - API Retrieving CodeReview Message Description
     H*******************************************************************
     H*  Description:                                                   *
     H*                                                                 *
     H*  This API will try to match a CodeReview Eob code with it's     *
     H*  associated CodeReview message and return the description.      *
     H*                                                                 *
     H*****************************************************************
     H*  Created on 11/12/2002 by c:62 for Project 114MDC
     H*******************************************************************
     H*                                                                 *
     H*  Called From:                                                   *
     H*    F0340R: WCMC EOMB Print Program                              *
     H*    W7002R: MICR Check Print Program                             *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*    HPRBMSA: CodeReview Message Program                          *
     H*                                                                 *
     H*******************************************************************
     H*  REVISIONS:
     H*                                                                 *
     H*******************************************************************
     FFDDEP     IF   E             DISK
     FFDMGL2    IF   E           K DISK
     FFTEBLCR   IF   E           K DISK
     FFTMIL     IF   E           K DISK
     FFDCHL1    IF   E           K DISK
     FFDDAL2    IF   E           K DISK
     FFDDHL1    IF   E           K DISK
     F*

     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D HPRMSG          DS
     D  CLOSE                  1      1
     D  AFLAG                  2      2
     D  APGMID                 3     10
     D  APARID                11     14
     D  AFILID                15     21
     D  FILL1                 22     30
     D  AFILES                31     32
     D  FILL2                 33     34
     D  AFILEF                35     46
     D  AARECK                47     86
     D  EOB#                  87    128
     D  DETAIL               129    131
     D  CODE                 132    136
     D  MOD                  137    138
     D  KEY                  139    143
     D  STATUS               144    144
     D  MSG1A                145    154
     D  MSG1B                155    220
     D  MSG2                 221    286
     D  MSG3                 287    352
     D  MSG4                 353    418
     D  MSG5                 419    484
     D  BLANK1               485    600
     D  BLANK2               601    800
     D  BLANK3               801   1000
     D  BLANK4              1001   1200
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@NOT1           70
     C                   PARM                    @@NOT2           70
     C                   PARM                    @@RTCD            1
     C*
     C     CHKY1         KLIST
     C                   KFLD                    DECO#
     C                   KFLD                    DESUB#
     C                   KFLD                    DEFND
     C                   KFLD                    DEFYR
     C                   KFLD                    DECASE
     C                   KFLD                    DEEOB#
     C                   KFLD                    DEDSEQ
     C*
     C     CHKY2         KLIST
     C                   KFLD                    DECO#
     C                   KFLD                    DESUB#
     C                   KFLD                    DEFND
     C                   KFLD                    DEFYR
     C                   KFLD                    DECASE
     C                   KFLD                    DEEOB#
     C                   KFLD                    DEDSEQ
     C                   KFLD                    KKCRB#
     C*
     C     DAKY          KLIST
     C                   KFLD                    DECO#
     C                   KFLD                    DESUB#
     C                   KFLD                    DEFND
     C                   KFLD                    DEFYR
     C                   KFLD                    DECASE
     C                   KFLD                    DEEOB#
     C                   KFLD                    DEDSEQ
     C                   KFLD                    DERCN
     C*
     C     DHKY          KLIST
     C                   KFLD                    DECO#
     C                   KFLD                    DESUB#
     C                   KFLD                    DEFND
     C                   KFLD                    DEFYR
     C                   KFLD                    DECASE
     C                   KFLD                    DEEOB#
     C                   KFLD                    DEDSEQ
     C*
     C     EBKY          KLIST
     C                   KFLD                    DECO#
     C                   KFLD                    KKKEY
     C*
     C     MGKY          KLIST
     C                   KFLD                    DECO#
     C                   KFLD                    DESUB#
     C                   KFLD                    DEFND
     C                   KFLD                    DEFYR
     C                   KFLD                    DECASE
     C                   KFLD                    DEEOB#
     C                   KFLD                    DEDSEQ
     C*
     C     MIKY          KLIST
     C                   KFLD                    KKMSG#
     C*
     C* FIELD DEFINITIONS
     C*
     C     *LIKE         DEFINE    CHCRB#        KKCRB#
     C     *LIKE         DEFINE    EBKEY         KKKEY
     C     *LIKE         DEFINE    MIMSG#        KKMSG#
     C*
     C*****************************************************************
     C**                      MAIN PROGRAM                           **
     C*****************************************************************
     C*
     C                   MOVEL(P)  'F'           @@RTCD
     C*
     C     @@RRN         CHAIN     PDE01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C* Let's get the CodeReview Edit Type
     C*
     C                   MOVEL(P)  DEEOB         KKKEY
     C     EBKY          CHAIN     PEB01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C* Let's read thru all messages for this line item
     C*
     C     MGKY          CHAIN     PMG01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C* Let's get the messages edit type and compare it to the eob
     C* codes edit type.
     C*
     C                   MOVEL(P)  MGMSG#        KKMSG#
     C     MIKY          CHAIN     PMI01                              99
     C     *IN99         IFEQ      *OFF
     C     MICRET        IFEQ      EBCRET
     C*
     C* Now let's call the CodeReview Message Program
     C*
     C                   CLEAR                   HPRMSG
     C                   MOVEL     'N'           CLOSE
     C                   MOVEL     'N'           AFLAG
     C                   MOVEL     DHPRCD        CODE
     C*
     C                   MOVE      *HIVAL        KKCRB#
     C     CHKY2         SETGT     PCH01
     C     CHKY1         READPE    PCH01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     CHCRPC        CODE
     C                   ENDIF
     C*
     C     DHKY          CHAIN     PDH01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     *BLANKS       DHMOD1
     C                   ENDIF
     C*
     C                   MOVEL     DHMOD1        MOD
     C                   MOVE      MGMSG#        KEY
     C*
     C     DAKY          CHAIN     PDA01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     *BLANKS       DACRST
     C                   ENDIF
     C*
     C                   MOVEL     DACRST        STATUS
     C*
     C                   CALL      'HPRBMSA'
     C                   PARM                    HPRMSG
     C*
     C                   MOVEL(P)  MSG1B         @@NOT1
     C                   MOVEL(P)  MSG2          @@NOT2
     C                   MOVEL     *BLANKS       @@RTCD
     C                   LEAVE
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C     MGKY          READE     PMG01                                  99
     C                   ENDDO
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C                   RETURN
