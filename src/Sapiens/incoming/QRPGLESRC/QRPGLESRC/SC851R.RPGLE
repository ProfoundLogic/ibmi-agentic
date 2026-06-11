     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H***************************************************************
     H*                                                            **
     H* PROGRAM:     SC851R                                        **
     H* DESCRIPTION: SELECT SMPRP DEPENDING ON DARTS RECORDS       **
     H*              SDCMP                                         **
     H* CREATED BY:  C:12   PROJ:OP2      DATE:  05/04/93          **
     H***************************************************************
     H* REVISIONS:                                                 **
     H**  C:02    120IAD   -  MODIFY TO GET THE SMOCP FILE TO RETRIEVE
     H**    01/26/04          THE OPTICAL LIBRARY NAME
     H**
     H***************************************************************
     H* INDICATOR USAGE:                                           **
     H*  60                    CHAIN/DOWEQ                         **
     H***************************************************************
     FSDCMP     IF   E             DISK
     FSMPRL1    UF A E           K DISK
     F**** ADD 01/26/04 ***********************************************
     FSMOCP     IF   E             DISK
     F                                     RENAME(POC01:POC02)
     F**** END ADD 01/26/04 *******************************************

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
     C                   TIME                    DSDTTM
     C*** 01/26/04 ** ADD *****************************************8
     C     1             CHAIN     SMOCP                              64
     C     *IN64         IFEQ      *OFF
     C                   MOVEL     OCOSYS        QQOSYS            1
     C                   MOVEL     OCLIBN        QQLIBN           10
     C                   ENDIF
     C*** 01/26/04 ** END ADD *************************************8
     C*
     C*   READ REMARK DETAIL RECORD
     C*
     C                   READ      PCM01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*                                                       DONE IN OPNQRY
     C***********CMFL05    IFNE 'O'                           DOC LOCAT
     C*                                                      CANT BE OP
     C     PRKY          CHAIN     PPR01                              40
     C                   MOVE      CMKEY         PRKEY
     C                   MOVE      CMSEQ#        PRSEQ#
     C                   MOVE      *BLANKS       PRSYS
     C                   MOVE      *BLANKS       PRHCMD
     C                   MOVE      *BLANKS       PRCPRY
     C*** CHANGE 01/26/04 ****************************
     C**** DEL             MOVE 'G'       PROSYS
     C                   MOVEL     QQOSYS        PROSYS
     C*** END CHANGE 01/26/04 ****************************
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
     C*********************ENDIF                           ENDIF NOT OP
      *
     C*
     C*** READ NEXT RECORD
     C                   READ      PCM01                                  60
     C                   ENDDO                                                  ENDDOW *IN60
     C*
     C*** END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
