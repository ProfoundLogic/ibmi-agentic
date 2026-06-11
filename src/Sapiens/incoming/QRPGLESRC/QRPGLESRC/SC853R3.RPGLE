     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***************************************************************
     H*                                                            **
     H* PROGRAM:     SC853R3                                       **
     H* DESCRIPTION: SELECT DARTS RECORDS FOR KEY PASSED IN. WRITE **
     H*               TO THE SMPRP FILE THE RECORDS SELECTED       **
     H*               CALLED BY: SC853R1                           **
     H*                          SC853R2                           **
     H* CREATED BY:  C:02   PROJ:129IMD   DATE:  10/07/05          **
     H***************************************************************
     H* REVISIONS:                                                 **
     H**
     H***************************************************************
     H* INDICATOR USAGE:                                           **
     H*  60                    CHAIN/DOWEQ                         **
     H***************************************************************
     FSDCMLE    IF   E           K DISK
     FSMPRL1    UF A E           K DISK
     FSMOCP     IF   E             DISK
     F                                     RENAME(POC01:POC02)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
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
     C     *LIKE         DEFINE    CMCDAT        XXRDAT
     C     *LIKE         DEFINE    CMCDAT        X1RDAT
      *
      *
     C     PRKY          KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     C*
     *** ENTRY PARM LIST ***
     C     *ENTRY        PLIST
     C                   PARM                    @@KEY            40
     C                   PARM                    @@RTN             1
     **
     C                   TIME                    DSDTTM
      *
      *** UPDATE THE CONTROL FILE INFO ****************************
      *
     C     1             CHAIN     SMOCP                              64
     C     *IN64         IFEQ      *OFF
     C                   MOVEL     OCOSYS        QQOSYS            1
     C                   MOVEL     OCLIBN        QQLIBN           10
     C                   ENDIF
     C*
     C*** CHAIN AND LOOP THROUGH ALL SDCM RECORDS WITH THE KEY THAT WAS PASSED
     C**** INTO THIS PROGRAM
     **
     ***
     **
     C     @@KEY         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     **
     C*   READ REMARK DETAIL RECORD
     C*
     C**********DEL ****   READ PCM01                    60
     C*
     C**** DEL ***IN60     DOWEQ'0'
     C*                                                       DONE IN OPNQRY
     C     PRKY          CHAIN     PPR01                              40
     C                   MOVE      CMKEY         PRKEY
     C                   MOVE      CMSEQ#        PRSEQ#
     C                   MOVE      *BLANKS       PRSYS
     C                   MOVE      *BLANKS       PRHCMD
     C                   MOVE      *BLANKS       PRCPRY
     C                   MOVEL     QQOSYS        PROSYS
     C                   MOVE      *BLANKS       PROFLD
     C                   MOVE      *BLANKS       PROSBD
     C                   MOVE      *BLANKS       PRFOLD
     C                   MOVE      *BLANKS       PRSUBD
     C     CMIMGF        IFEQ      'Y'
     C                   MOVEL     CMIDOC        PRDCID
     C                   ELSE
     C                   MOVEL     CMDCID        PRDCID
     C                   ENDIF
     C                   MOVE      *BLANKS       PRDELF
     C                   MOVE      CMPNAM        PRPNAM
     C                   MOVE      *BLANKS       PRASTT
     C                   MOVE      *BLANKS       PRTSFL
     C                   Z-ADD     0             PRBUL#
     C                   MOVE      *BLANKS       PRERDS
     C                   MOVE      'P'           PRSTAT
     C*
     C                   MOVE      Q1USER        PRUSER
     C                   MOVEL     Q1PGM         PRPGM
     C                   Z-ADD     DSTIME        PRTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PRDT
     C*
     C                   MOVE      Q1USER        PRIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PRIDT
      *
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PPR01
     C                   ELSE
     C                   WRITE     PPR01
     C                   ENDIF
      *
      *
     C*
     C*** READ NEXT RECORD
     C     @@KEY         READE     PCM01                                  60
     C                   ENDDO                                                  ENDDOW *IN60
     C*
     C*** END THE PROGRAM
     C*** RETURN TO SAVE OPEN DATA PATHS
     C                   RETURN
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
