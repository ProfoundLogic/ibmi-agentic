     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - FA704RC - UPDATE WCMC NAME FIELDS ON CHANGE              DATA
     H*******************************************************************
     H*  Description:  UPDATE WCMC NAME FIELDS ON CHANGE               D*TA
     H*
     H*           >>>  BATCH / CONVERSION VERSION  <<<
     H*                                                                 *
     H*  Called From:                                                   *
     H*                                                                 *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*    None                                                         *
     H*                                                                 *
     H*  Created:  12/26/97  C:53  Project:  MC145                      *
     H*                                                                 *
     H*******************************************************************
     H*
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*   1/17/01 C:58 PROJECT: MCXXX
     H*            ADD PARM RTCD = '*' -- To close files at end of
     H*                                   batch processing.
     H*
     H* 07/19/00 C:62 Req/Proj: MC048 23470
     H*               NEED TO RESET COMPANY POINTER.
     H*
     H* 01/31/00 C:53 Req/Proj: MCMNT #20871
     H*               ADD NAME UPDATES FOR FAEOP, FALTP
     H*               SUBSTITUTED STANDARD DATE CONVERSION ROUTINES.
     H*
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       60      GENERAL USAGE (CHAIN)                           *
     H*                                                               *
     H*****************************************************************
     F*
     FFMCMLA    UF   E           K DISK
     F*** BEG ADD *** 01/31/00 ******************************
     FFAEOLJ    UF   E           K DISK
     FFALTL4    UF   E           K DISK
     F*** BEGIN CHG *** 07/19/00 **************************************
     F**********SMCOP   IF  E           K        DISK
     FSMCOP     IF   E             DISK
     F*** END   CHG *** 07/19/00 **************************************
     F*** END ADD *** 01/31/00 ******************************

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@SS#             9
     C                   PARM                    @@LSN            30
     C                   PARM                    @@FSN            15
     C                   PARM                    @@MID             1
     C**** BEG ADD ***  1/17/01 ***************************************
     C                   PARM                    @@RTCD            1
     C**** END ADD ***  1/17/01 ***************************************
     C*
     C*** BEG ADD *** 01/31/00 ******************************
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
     C     EOKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSS#
     C*
     C     *LIKE         DEFINE    CMDT          XXDT
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C*** END ADD *** 01/31/00 ******************************
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C*
     C**** BEG ADD ***  1/17/01 ***************************************
     C*   IF PARM RTCD = '*' END PGM AND RETURN
     C*
     C     @@RTCD        IFEQ      '*'
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF
     C**** END ADD ***  1/17/01 ***************************************
     C*
     C                   CLEAR                   PCM01
     C                   CLEAR                   PEO01
     C                   CLEAR                   PLT01
     C*
     C                   MOVE      @@SS#         KKSS#
     C*
     C*** BEG ADD *** 01/31/00 ******************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*** END ADD *** 01/31/00 ******************************
     C*
     C*** LOOP THRU CLAIM MASTER AND UPDATE
     C*
     C     KKSS#         SETLL     PCM01
     C     KKSS#         READE     PCM01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C*** MOVE NAME FIELDS
     C*
     C                   MOVE(P)   @@LSN         CMLSN
     C                   MOVE(P)   @@FSN         CMFSN
     C                   MOVE(P)   @@MID         CMMID
     C*
     C                   MOVE      Q1USER        CMUSER
     C                   MOVE      Q1PGM         CMPGM
     C*** BEG CHG *** 01/31/00 ******************************
     C*******************  TIME           DSDTTM
     C*******************  Z-ADDDSTIME    CMTIME
     C***********DSDATE**  MULT 10000.01  CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C                   Z-ADD     XXDT          CMDT
     C*** END CHG *** 01/31/00 ******************************
     C*
     C                   UPDATE    PCM01
     C     KKSS#         READE     PCM01                                  60
     C*
     C                   ENDDO
     C*** BEG ADD *** 01/31/00 ******************************
     C*
     C*** LOOP THRU THE EOMB FILE AND UPDATE
     C*
     C*** GET COMPANY #
     C*** BEGIN ADD *** 07/19/00 **************************************
     C     1             SETLL     PCO01
     C*** END   ADD *** 07/19/00 **************************************
     C                   READ      PCO01                                  90
     C     *IN90         DOWEQ     *OFF
     C                   Z-ADD     COCO#         KKCO#
     C*
     C     EOKEY         SETLL     PEO01
     C     EOKEY         READE     PEO01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C*** MOVE NAME FIELDS
     C*
     C                   MOVE(P)   @@LSN         EOLSN
     C                   MOVE(P)   @@FSN         EOFSN
     C                   MOVE(P)   @@MID         EOMID
     C*
     C******************** MOVE Q1USER    EOUSER *** DO NOT UPDATE ***
     C                   MOVE      Q1PGM         EOPGM
     C                   Z-ADD     XXDT          EODT
     C*
     C                   UPDATE    PEO01
     C     EOKEY         READE     PEO01                                  60
     C*
     C                   ENDDO
     C*
     C                   READ      PCO01                                  90
     C                   ENDDO
     C*
     C*** LOOP THRU THE LETTER FILE AND UPDATE
     C*
     C     KKSS#         SETLL     PLT01
     C     KKSS#         READE     PLT01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C*** MOVE NAME FIELDS
     C*
     C                   MOVE(P)   @@LSN         LTLNM
     C                   MOVE(P)   @@FSN         LTFNM
     C*
     C                   MOVE      Q1USER        LTUSER
     C                   MOVE      Q1PGM         LTPGM
     C                   Z-ADD     DSTIME        LTTIME
     C                   Z-ADD     XXDT          LTDT
     C*
     C                   UPDATE    PLT01
     C     KKSS#         READE     PLT01                                  60
     C*
     C                   ENDDO
     C*** END ADD *** 01/31/00 ******************************
     C*
     C     TERM          TAG
     C                   RETURN
