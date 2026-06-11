     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IDARTR - WRITE DARTS RECORD
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  SUBMITTED FROM:                                              *
     H*    I0020R - SEND FAX PROGRAM (DISPLAY)                        *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  PROGRAMMER:                                                  *
     H*  CREATED:                         PROJECT: AOK                *
     H******************************************************************
     H*  REVISION:                                                    *
     H*                                                               *
     H*   10/17/11 C:279 Project: SY195                               *
     H*                  - Modify program to pass/return UNC path     *
     H*                    field and IFS flag to called/calling       *
     H*                    programs.                                  *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H*  02/23/98 C:85 Project: WC577                                 *
     H*                CHANGED THE FIELD MOVED TO CMSMCD              *
     H*                FROM *BLANKS TO CTSMCD.                        *
     H*                                                               *
     H*  11/16/92 C:10 PROJ:                                          *
     H*           ADDED CODE TO INITIALIZE THE FOLLOWING DARTS 3.0    *
     H*           FIELDS FOR SDCMP: CMSMCD, CMFL01, CMFL02, CMFL03,   *
     H*                             CMFL04, CMFL05, CMIDT             *
     H*                                                               *
     H*  06/15/92 TAA  PROJ:  AOK                                     *
     H*           USE HARD-CODED 'PSQF' FOR UPDATE OF CMRMCD (5A)     *
     H*           INSTEAD OF UPDATING CMCOMM WITH 'QF' (2A)           *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H******************************************************************
     H* INDICATOR USAGE                                               **
     H*  60    CHAIN & READ                                           **
     H******************************************************************
     H*  FILE USAGE:                                                  *
     H*****************************************************************
     F*                                                                *********
     F*** BEGIN ADD *** 02/23/98 ***********************************************
     FSTCTL1    IF   E           K DISK
     F*** END   ADD *** 02/23/98 ***********************************************
     F*                                                                *********
     FSDCML     O    E           K DISK
     FSDCMLA    UF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
      ***** END ADD ***** 10/17/11 ***********************************

     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTFL                40     40
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     F*

     IPCM02
     I              CMKEY                       C2KEY
     I              CMCSID                      C2CSID
     I              CMCOMM                      C2COMM
     I              CMCDAT                      C2CDAT
     I              CMCODE                      C2CODE
     I              CMDCID                      C2DCID
     I              CMPNAM                      C2PNAM
     I              CMTRDT                      C2TRDT
     I              CMTRTM                      C2TRTM
     I              CMSEQ#                      C2SEQ#
     I              CMSECL                      C2SECL
     I              CMSBYL                      C2SBYL
     I              CMSTAT                      C2STAT
     I              CMUSER                      C2USER
     I              CMPGM                       C2PGM
     I              CMTIME                      C2TIME
     I              CMDT                        C2DT
     I              CMIUSR                      C2IUSR
     I              CMFOLD                      C2FOLD
     I              CMDCDT                      C2DCDT
     I              CMDIFL                      C2DIFL
     I*
     I              CMAPID                      C2APID
     I              CMIDOC                      C2IDOC
     I              CMIQUE                      C2IQUE
     I              CMIMGF                      C2IMGF
     I              CMIPTH                      C2IPTH
     I              CMPIDX                      C2PIDX
     I              CMQPTY                      C2QPTY
     I              CMDCTP                      C2DCTP
     I              CMIPGM                      C2IPGM
     I*
     I              CMSMCD                      C2SMCD
     I              CMFL01                      C2FL01
     I              CMFL02                      C2FL02
     I              CMFL03                      C2FL03
     I              CMFL04                      C2FL04
     I              CMFL05                      C2FL05
     I              CMIDT                       C2IDT
      ***** BEG ADD ***** 10/17/11 ***********************************
     I              CMUNCN                      C2UNCN
     I              CMSHRN                      C2SHRN
     I              CMIFSP                      C2IFSP

     C*
     C     *LIKE         DEFINE    CMDT          XXDATE
     C     *LIKE         DEFINE    QQEMP#        XXEMP#
     C     *LIKE         DEFINE    QQDIV         XXDIV
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQDESC           40
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM                    QQRTCD            1
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   PARM                    QQUNCN          255
     C                   PARM                    QQIFSP            1
      ***** END ADD ***** 10/17/11 ***********************************
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
     C*** DARTS FILE
     C     C2KY          KLIST
     C                   KFLD                    QCKEY
     C                   KFLD                    ##SEQ#            5 0
     C*
     C*
     C*
     C*
     C                   MOVE      QQCO#         XXCO#             3
     C                   MOVE      QQFND         XXFND             3
     C                   MOVE      QQFYR         XXFYR             3
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQDESC        XXDESC           40
     C                   MOVE      QQDCID        XXDCID           12
     C                   MOVE      QQFOLD        XXFOLD           80
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C*
     C                   MOVE      ' '           QQRTCD
     C*
     C*
     C*
     C*** WRITE RECORD TO DARTS ******************************************
     C*
     C*** ESTABLISH KEY
     C                   MOVE      *BLANKS       CMKEY
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      XXEMP#        QCEMP#
     C                   MOVE      XXDIV         QCDIV
     C                   MOVE      'Q'           QCQTFL
     C*** ESTABLISH RECORD
     C                   MOVEL     QCKEY         CMKEY
     C                   MOVE      *BLANKS       CMCSID
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMCDAT
     C*
     C                   MOVE      *BLANKS       CMCOMM
     C                   MOVE      'PSQF'        CMRMCD
     C*                                                                *********
     C*** BEGIN ADD *** 02/23/98 ***********************************************
     C*                                                                *********
     C     CMRMCD        CHAIN     PCT01                              60
     C*                                                                *********
     C     *IN60         IFEQ      *OFF
     C                   MOVE      CTSMCD        CMSMCD
     C                   ENDIF                                                  *IN60=OFF
     C*                                                                *********
     C*** END   ADD *** 02/23/98 ***********************************************
     C*
     C                   MOVEL     XXDESC        CMCOMM
     C*
     C* W00050R IS KEY PGM USED FOR SMKDP KEY DEFINITION FILE
     C                   MOVEL     'W0050R'      CMPNAM                         KEYED PGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMTRDT
     C*
     C                   Z-ADD     DSTIME        CMTRTM
     C                   MOVE      *ZEROS        CMSECL
     C                   MOVE      'N'           CMSBYL
     C                   MOVE      *BLANKS       CMSTAT
     C                   MOVE      Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*
     C                   MOVE      Q1USER        CMIUSR
     C                   MOVE      XXFOLD        CMFOLD
     C                   MOVE      *ZEROS        CMDCDT
     C                   MOVE      'N'           CMDIFL
     C*
     C                   MOVE      XXDCID        CMDCID
     C                   MOVE      'WC'          CMAPID
     C                   MOVE      *BLANKS       CMIDOC
     C                   MOVE      *BLANKS       CMIQUE
     C                   MOVE      *BLANKS       CMIMGF
     C                   MOVE      *BLANKS       CMIPTH
     C                   MOVE      *ZEROS        CMPIDX
     C                   MOVE      *BLANKS       CMQPTY
     C                   MOVE      *BLANKS       CMDCTP
     C                   MOVEL     Q1PGM         CMIPGM
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   MOVEL     QQUNCN        CMUNCN
     C                   MOVEL     QQIFSP        CMIFSP
      ***** END ADD ***** 10/17/11 ***********************************
     C*
     C*** GET LAST SEQ# USED
     C                   Z-ADD     *HIVAL        ##SEQ#
     C     C2KY          SETGT     PCM02
     C                   READP     PCM02                                  50
     C  N50              EXCEPT    PCM02E
     C     *IN50         IFEQ      '0'
     C     QCKEY         ANDEQ     C2KEY
     C     C2SEQ#        ADD       1             CMSEQ#
     C                   ELSE
     C                   Z-ADD     1             CMSEQ#
     C                   END
     C*                                                                *********
     C*** BEGIN DEL *** 02/23/98 ***********************************************
     C**********           MOVE *BLANKS   CMSMCD
     C*** END   DEL *** 02/23/98 ***********************************************
     C*                                                                *********
     C                   MOVE      *BLANKS       CMFL01
     C                   MOVE      *BLANKS       CMFL02
     C                   MOVE      *BLANKS       CMFL03
     C                   MOVE      *BLANKS       CMFL04
     C                   MOVE      *BLANKS       CMFL05
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMIDT
     C*
     C                   WRITE     PCM01
     C***** BEGIN ADD **** 07/01/99 ***********************************
     C                   FEOD      SDCML
     C***** END   ADD **** 07/01/99 ***********************************
     C*
     C*
     C*  END THE PROGRAM
     C                   MOVE      '1'           *INLR
     C*
     OPCM02     E            PCM02E
