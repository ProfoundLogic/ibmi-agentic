     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *****************************************************************
      *TITLE SC821R - OPTICAL PURGE/DELETE SELECTION PROGRAM
      *****************************************************************
      *    PROGRAM DESCRIPTION:                                       *
      *        BACKUP AND BATCH - Optical Select Documents from Darts *
      *                           Records (SDCMP) and fill SMPWP      *
      *                                                               *
      *  CALLED FROM: SC820C                                          *
      *                                                               *
      *  CALLS TO:                                                    *
      *                                                               *
      *  CREATED: 03/31/03    PROGROGRAMMER: C:145    PROJECT: 124IAA *
      *****************************************************************
      ** REVISIONS:                                                   *
      *  CREATED:             PROGRAMMER:             PROJECT:        *
      *                                                               *
      *  03/31/02             C:145                   124IAA          *
      *                Original program for optical purge/delete      *
      *                selection program was SC851R.  Program renamed *
      *                and modified to reflect changes for new        *
      *                batch/backup process.                          *
      ******************************************************************
      * INDICATOR USAGE                                                *
      *                                                                *
      *  60                    CHAIN/DOWEQ                         **
      ******************************************************************
      *
     FSDCMP     IF   E             DISK
     FSMPWL2    UF A E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C     *LIKE         DEFINE    CMCDAT        XXRDAT
     C     *LIKE         DEFINE    CMCDAT        X1RDAT
      *
      *
     C     PWKY          KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
      *
     C                   TIME                    DSDTTM
      *
      * Read remark detail record
      *
     C                   READ      PCM01                                  60
      *
     C     *IN60         DOWEQ     '0'
      *                                                       DONE IN OPNQRY
     C***********CMFL05    IFNE 'O'                           DOC LOCAT
      *                                                      CANT BE OP
     C     PWKY          CHAIN     PPW01                              40
     C                   MOVE      CMKEY         PWKEY
     C                   MOVE      CMSEQ#        PWSEQ#
     C                   MOVE      *BLANKS       PWSYS
     C                   MOVE      *BLANKS       PWHCMD
     C                   MOVE      *BLANKS       PWCPRY
     C                   MOVE      'G'           PWOSYS
     C                   MOVE      *BLANKS       PWOFLD
     C                   MOVE      *BLANKS       PWOSBD
     C                   MOVE      *BLANKS       PWFOLD
     C                   MOVE      *BLANKS       PWOBSD
     C                   MOVE      *BLANKS       PWOBFL
     C                   MOVE      *BLANKS       PWOBDI
     C                   MOVE      *BLANKS       PWSUBD
     C     CMIMGF        IFEQ      'Y'
     C                   MOVEL     CMIDOC        PWDCID
     C                   ELSE
     C                   MOVEL     CMDCID        PWDCID
     C                   ENDIF
     C                   MOVE      *BLANKS       PWDELF
     C                   MOVE      CMPNAM        PWPNAM
     C                   MOVE      *BLANKS       PWASTT
     C                   MOVE      *BLANKS       PWTSFL
     C                   Z-ADD     0             PWBUL#
     C                   MOVE      *BLANKS       PWERDS
     C                   MOVE      'P'           PWSTAT
      *
     C                   MOVE      Q1USER        PWUSER
     C                   MOVEL     Q1PGM         PWPGM
     C                   Z-ADD     DSTIME        PWTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PWDT
      *
     C                   MOVE      Q1USER        PWIUSR
      *
      * Need Darts transaction date to be the initial date of the PW
      * record
      *
     C                   MOVE      CMTRDT        PWIDT
      *
      * If record already exists, update it.
      *
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PPW01
      *
      * else, write the new record to the PW file.
      *
     C                   ELSE
     C                   WRITE     PPW01
     C                   FEOD      SMPWL2
     C                   ENDIF
      *
     C*********************ENDIF                           ENDIF NOT OP
      *
      *
      * Read Next Record
      *
     C                   READ      PCM01                                  60
     C                   ENDDO                                                  ENDDOW *IN60
      *
      * End the Program
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
