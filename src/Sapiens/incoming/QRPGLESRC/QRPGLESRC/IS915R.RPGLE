     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS915R   REMOVE ALL "DELETED" IMAGES FROM SYSTEM
     H*****************************************************************
     H* THIS PROGRAM REMOVES THE DELETED IMAGES/FAXES FROM THE SYSTEM *
     H*                                                               *
     H*    CALLS TO:                                                  *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
      *  10/17/11 C:279 Project: SY195
      *                 Modify program to pass/return UNC path field
      *                 and IFS flag to called/calling programs.
      *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* 60  GENERAL CHAIN INDICATOR                                   *
     H*****************************************************************
     H** FILES:                                                      **
     H*****************************************************************
     F*
     FIMDOL8    UF   E           K DISK
     FIMIFL2    UF   E           K DISK
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  DSDTT1                 1     14  0
     D  DSTIM1                 1      6  0
     D  DSDAT1                 7     14  0
     D  DSMM                   7      8  0
     D  DSDY                   9     10  0
     D  DSYR                  11     14  0
     D                 DS
     D  XXSDT                  1      8  0
     D  XXYR                   1      4  0
     D  XXMM                   5      6  0
     D  XXDY                   7      8  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     IFKY          KLIST
     C                   KFLD                    DODCID
     C*
     C                   TIME                    DSDTT1
     C                   MOVE      DSYR          XXYR
     C                   MOVE      DSDY          XXDY
     C** IF MONTH IS JAN, MUST SET PRIOR YEAR FOR DEC.
     C     DSMM          IFEQ      1
     C                   Z-ADD     12            XXMM              2 0
     C     DSYR          SUB       1             XXYR
     C                   ELSE
     C     DSMM          SUB       1             XXMM
     C                   ENDIF
     C*
     C     *LOVAL        SETLL     PDO01
     C                   READ      PDO01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     DOIDFX        IFNE      'Y'
     C     DOSDT         ANDLE     XXSDT
     C     IFKY          CHAIN     PIF01                              61
     C   61              CLEAR                   PIF01
     C*
     C**** CALL CL TO RMVM FOR FAX AND TO DLTDLO FOR DOCS
     C*
     C                   CALL      'IS915C'
     C                   PARM                    DODCID
     C                   PARM                    DOFLR
     C                   PARM                    DOSFLR
     C                   PARM                    IFLFM
     C                   PARM      *BLANKS       QQRTN             2
     C*** BEG ADD *** 10/17/11 ****************************************
     C                   PARM                    DOUNCN
     C                   PARM                    DOIFSP
     C*** END ADD *** 10/17/11 ****************************************
     C*
     C*** IF FAX MBR RMVD, WE UPDATE IF WITH A "D" IN IFSTAT
     C*** IF DOC REMOVED WE UPDATE THE DO RCD WITH "D" IN DOFL02
     C*
     C                   MOVEL     QQRTN         RTNF              1
     C                   MOVE      QQRTN         RTND              1
     C*
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      XXDT              6 0
     C                   Z-ADD     XXDT          DOLDT
     C                   Z-ADD     DSTIME        DOLTIM
     C                   MOVEL     Q1USER        DOLUSR
     C                   MOVEL     Q1PGM         DOLPGM
     C*
     C                   MOVE      DOLPGM        IFPGM
     C                   MOVE      DOLUSR        IFUSER
     C*
     C     RTND          IFEQ      ' '
     C                   MOVEL     'D'           DOFL02
     C                   UPDATE    PDO01
     C                   ENDIF
     C     RTNF          IFEQ      ' '
     C                   MOVEL     'D'           IFSTAT
     C  N61              UPDATE    PIF01
     C                   ENDIF
     C*
     C                   ENDIF
     C                   READ      PDO01                                  60
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
