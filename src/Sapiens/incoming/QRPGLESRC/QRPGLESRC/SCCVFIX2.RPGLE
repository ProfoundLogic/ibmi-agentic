     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      ***************************************************************
      * CALLS TO/PARMS
      *
      ***************************************************************
      *  REVISIONS:
     H***************************************************************
     FSXC1LA    IF   E           K DISK
     F                                     RENAME(PC101:PC10A)
     FSXC1LB    IF   E           K DISK
     F                                     RENAME(PC101:PC10B)
     F*
     FSXC2LA    IF   E           K DISK
     F                                     RENAME(PC201:PC20A)
     FSXC2LB    IF   E           K DISK
     F                                     RENAME(PC201:PC20B)
     F*
     F*

     D*
      *
      *
     D @@FPTH          DS
     D  @FPRE                  1      5
     D  SVOLPT                 6     80
     D SPTHLN          DS
     D  SPTHL                  1      4B 0
     D*
     D @@TPTH          DS
     D  @TPRE                  1      5
     D  TVOLPT                 6     80
     D @@TPT2          DS
     D  @TPR2                  1      9
     D  TVOLP2                10     80
     D TPTHLN          DS
     D  TPTHL                  1      4B 0
     D*
     D CPYINF          DS
     D  TYPOFC                 1      1
     D  CPYFIL                 2      6
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      * * *  END PROGRAM CONTROL DATA AREA (NON BLANK MEANS END PROGRAM)
      * * *    X#FLG = THIS PROGRAM CONTROL  X#FLG2 = DELETE PROCESS CTL
     D SCCV           UDS
     D  X#FLG                  1      1
     D  X#FLG2                 2      2
     D  X#BLNK                 3    256
     D*
     D                 DS
     D  ERDS                   1     40
     D  ERPGM                  1      8
     D  ERBLK1                 9      9
     D  ERRTCD                10     10
     D  ERBLK2                11     11
     D  ERDESC                12     40
      * =================================== *
      * * *  KEY LISTS                      *
      * =================================== *

     C*
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
      *
     C     PLCV1         PLIST
     C                   PARM      CVNDOC        ZZDCID           12
     C                   PARM      CVNFLD        ZZFOLD           80
     C                   PARM      DLKEY         ZZKEY            40
     C                   PARM                    ZZSEQ#            5
     C                   PARM      'O'           ZZOPTF            1
     C                   PARM      *BLANK        ZZRTCD            1
      *
     C*** DARTS RECORD
      *
     C     CMKY          KLIST
     C                   KFLD                    DLKEY
     C                   KFLD                    DLSEQ#
      *
     C     CMK1          KLIST
     C                   KFLD                    OLKEY
     C                   KFLD                    OLSEQ#
      *
     C*
     C*** DOCUMENT LOCATION FILE
      *
     C     DLK           KLIST
     C                   KFLD                    CVNDOC
     C                   KFLD                    CVNFLD
     C*
     C*
     C*** DOCUMENT LOCATION FILE
      *
     C     OLK           KLIST
     C                   KFLD                    CVNDOC
     C                   KFLD                    CVNFLD
     C*
     C*** DOCUMENT LOCATION FILE
      *
     C     MDK           KLIST
     C                   KFLD                    OLKEY
     C                   KFLD                    OLSEQ#
     C                   KFLD                    OLOID
     C                   KFLD                    OLOFLR
     C*
      *
      *
     C*
      *
      * =================================== *
      * * *  PARM LISTS                     *
      * =================================== *
      *
     C*
     C*** ENTRY PARM LIST
     C     *ENTRY        PLIST
     C                   PARM                    QQVOL            10
     C                   PARM                    QQRTN             1
      *
      *
     C                   Z-ADD     0             COUNT             4 0
      *** READ EACH RECORD SELECTED FOR PURGE.
     C* *** SET TO FIRST "C" RCD FOR THE BEGINING VOLUME
     C     QQVOL         SETLL     PC10A
     C     QQVOL         READE     PC10A                                  60
     C*** CONTINUE PROCESSING AS LONG AS WE ARE WITHIN THE BEGIN/END
     C*** VOLUMES
     C     *IN60         DOWEQ     '0'
     C*
     ** FOR EACH RECORD (ERROR RECORD), WE'LL TRY TO "GET" A PRIOR RECORD, IF FO
     *** IF FOUND, WE'LL COPY FROM OPTICAL TO THE CVNDOC CVNFLD
     ***  *** DONE, THEN RUN THE NORMAL PURGE TO OPTICAL PROCESS (SCCV0R?)
     C*
     C     DLK           CHAIN     PC10B                              65
     C     *IN65         IFEQ      *OFF
     C*** GET DOC BACK TO DASD
     C****
     C                   EXSR      CVT
     C                   ENDIF
     C*
      *
     C     NXTONE        TAG
     C     QQVOL         READE     PC10A                                  60
     C                   ENDDO
     C***********************************************************************
     C     QQVOL         SETLL     PC20A
     C                   READ      PC20A                                  60
     C*** CONTINUE PROCESSING AS LONG AS WE ARE WITHIN THE BEGIN/END
     C*** VOLUMES
     C     *IN60         DOWEQ     '0'
     C*
     C*
     C*
     C     OLK           CHAIN     PC20B                              65
     C     *IN65         IFEQ      *OFF
     C*** GET DOC BACK TO DASD
     C                   ENDIF
     C*
     C*
      *
     C***
      *
     C     NXTON1        TAG
     C                   READ      PC20A                                  60
     C                   ENDDO
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     ***========================================================================
     C     CVT           BEGSR
     C                   MOVE      DLSEQ#        ZZSEQ#            5
     C*
     C                   MOVE      *BLANKS       ZZFOLD
     C                   MOVEL     CVNFLD        ZZFOLD
     C*
     C                   CALL      'SCCVFX2C'    PLCV1
     C*
     C                   ENDSR
