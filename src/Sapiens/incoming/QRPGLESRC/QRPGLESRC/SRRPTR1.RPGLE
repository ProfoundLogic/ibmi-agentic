     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE SRRPTR1 - EMPLOYER REPORT WORKFILE - LOAD AGENT AS OF DATE
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION                                          *
     H*                                                               *
     H*    Ensures Agent# as part of each workfile, WXEBP, record is  *
     H*    the latest Agent in effect as of a run date.               *
     H*                                                               *
     H*    Workfile record removed if agent is inactive as of the run *
     H*    date or no agent found in Agent History file as of the run *
     H*    date.                                                      *
     H*                                                               *
     H*    This program will need to be run directly following SRRPTR.*
     H*                                                               *
     H*                                                               *
     H*  CREATED: 01/13/98        PROGRAMMER: 30       PROJECT: CM030 *
     H*                                                               *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*  05/26/10 C:235 PROJECT: 133WIP PRISS-21                      *
     H*                 Agent Employee List Report-Add Agent Status   *
     H*                 date                                          *
     H*                                                               *
     H*  05/06/10 C:235 PROJECT: 133WIP                               *
     H*                 Agent Employee List Report-Add Agent Status   *
     H*                 date                                          *
     H*                                                               *
     H* 01/30/08  C:33  Seq#: WA133/62106                             *
     H*                 Retrieve the servicing agent instead of the   *
     H*                  agent as of the run date.                    *
     H*****************************************************************
     H*                                                               *
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*      60    READ/CHAIN                                         *
     H*                                                               *
     H*****************************************************************
     FWXEBL     UF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK

     D*
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    EBCO#
     C                   KFLD                    EBFND
     C                   KFLD                    EBEMP#
     C                   KFLD                    EBDIV
     C                   KFLD                    EBFYR
     C                   KFLD                    X7EFDT
     C*
     C     AHKY1         KLIST
     C                   KFLD                    EBCO#
     C                   KFLD                    EBFND
     C                   KFLD                    EBEMP#
     C                   KFLD                    EBDIV
     C                   KFLD                    EBFYR
     C*****BEG ADD ****05/06/10***************************************
     C*
     C     AHKY2         KLIST
     C                   KFLD                    EBCO#
     C                   KFLD                    EBFND
     C                   KFLD                    EBEMP#
     C                   KFLD                    EBDIV
     C                   KFLD                    EBFYR
     C                   KFLD                    XXEFDT
     C*****END ADD ****05/06/10***************************************
     C*
     C*
     C*  KEY LIST FOR THE AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    AHCO#
     C                   KFLD                    AHFND
     C                   KFLD                    AHAGT#
     C*_______________________________________________________________
     C*
     C     *LIKE         DEFINE    EBCO#         XXCO#
     C     *LIKE         DEFINE    EBFND         XXFND
     C     *LIKE         DEFINE    EBEMP#        XXEMP#
     C     *LIKE         DEFINE    EBDIV         XXDIV
     C     *LIKE         DEFINE    EBFYR         XXFYR
     C     *LIKE         DEFINE    EBAGT#        SVAGT#
     C     *LIKE         DEFINE    AHEFDT        X7EFDT
     C*****BEG ADD ****05/06/10***************************************
     C     *LIKE         DEFINE    AHEFDT        XXEFDT
     C*****END ADD ****05/06/10***************************************
     C*_______________________________________________________________
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQEFDT            8
     C                   PARM                    QQSORT            2
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
     C*_______________________________________________________________
     C*
     C***  BEG   ADD ***  05/06/10  **********************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C***  END   ADD ***  05/06/10  **********************************
     C                   MOVE      QQEFDT        X8EFDT            8 0
     C*
     C                   Z-ADD     X8EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7EFDT
     C***  BEG  CHG *****  05/06/10  **************************************
     C***  Begin Add  ***  01/30/08  **************************************
     C*********            Z-ADD*HIVAL    X7EFDT
     C     CLTID         IFEQ      '133'
     C     X7EFDT        IFEQ      *ZEROS
     C                   Z-ADD     *HIVAL        X7EFDT
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     *HIVAL        X7EFDT
     C                   ENDIF
     C***  End   Add  ***  01/30/08  **************************************
     C***  END  CHG *****  05/06/10  **************************************
     C*
     C     Q1PRMS        IFGE      2
     C                   MOVEL(P)  QQSORT        XXSORT            2
     C                   ELSE
     C                   MOVEL     *BLANKS       XXSORT
     C                   ENDIF                                                  Q1PRMS>=2
     C*
     C                   READ      PEB01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     EBAGT#        SVAGT#
     C*
     C                   EXSR      SRACK
     C*
     C     XXUPD         IFEQ      'Y'
     C*** BEG CHG *** 05/06/10 *******************************************
     C******     AHAGT#    ANDNESVAGT#
     C     AHAGT#        IFNE      SVAGT#
     C*** BEG CHG *** 05/06/10 *******************************************
     C*
     C                   Z-ADD     AHAGT#        EBAGT#
     C*
     C     AAKY          CHAIN     PAA01                              60
     C  N60              MOVEL(P)  AANAME        EBANAM
     C*
     C                   SELECT
     C     XXSORT        WHENEQ    'AN'
     C                   MOVEL(P)  EBAGT#        EBSRT1
     C     XXSORT        WHENEQ    'AA'
     C                   MOVEL(P)  EBANAM        EBSRT1
     C     LO:UP         XLATE     EBSRT1        EBSRT1
     C                   ENDSL
     C*
     C                   MOVE(P)   Q1USER        EBUSER
     C                   MOVE(P)   Q1PGM         EBPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EBDT
     C*
     C                   Z-ADD     DSTIME        EBTIME
     C*
     C*
     C*** BEG ADD *** 05/06/10 *******************************************
     C     CLTID         IFEQ      '133'
     C                   MOVE(P)   Q1USER        EBUSER
     C                   MOVE(P)   Q1PGM         EBPGM
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EBDT
     C*
     C                   Z-ADD     DSTIME        EBTIME
     C*
     C                   Z-ADD     *HIVAL        XXEFDT
     C     AHKY2         SETGT     PAH01
     C     AHKY1         READPE    PAH01                                  60
     C*
     C                   MOVE      AHAGT#        EBAGTL
     C                   ENDIF
     C*** BEG ADD *** 05/25/10 *******************************************
     C                   UPDATE    PEB01
     C*** END ADD *** 05/25/10 *******************************************
     C*** END ADD *** 05/06/10 *******************************************
     C                   ELSE
     C     CLTID         IFEQ      '133'
     C                   Z-ADD     *HIVAL        XXEFDT
     C     AHKY2         SETGT     PAH01
     C     AHKY1         READPE    PAH01                                  60
     C*
     C                   MOVE      AHAGT#        EBAGTL
     C                   ENDIF
     C*** END ADD *** 05/06/10 *******************************************
     C*
     C                   UPDATE    PEB01
     C*
     C                   ENDIF                                                  CLTID=133
     C                   ENDIF                                                  XXUPD=Y
     C*
     C                   READ      PEB01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   SETON                                        LR
     C*_______________________________________________________________
     C*
     C*===============================================================
     C*  SRACK - CHECK AGENT IS LATEST AS OF RUN DATE
     C*===============================================================
     C*
     C     SRACK         BEGSR
     C*
     C                   MOVE      'N'           XXUPD             1            NO UPDATE
     C                   MOVE      'N'           XXDEL             1            DEL WRK REC
     C*
     C     AHKY          SETGT     PAH01
     C                   SETOFF                                       60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     AHKY1         READPE    PAH01                                  60
     C*
     C                   SELECT
     C     *IN60         WHENEQ    *ON
     C                   MOVE      'N'           XXUPD             1            NO UPDATE
     C                   MOVE      'D'           XXDEL             1            DEL WRK REC
     C     AHSTAT        WHENNE    'I'
     C                   MOVE      'Y'           XXUPD                          YES UPDATE
     C                   LEAVE
     C                   ENDSL
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C*  IF AGENT IS INACTIVE OR NON-EXISTENT REMOVE WORKFILE RECORD
     C*
     C     AHSTAT        IFEQ      'I'
     C     XXDEL         OREQ      'D'
     C                   DELETE    PEB01
     C                   ENDIF                                                  AHSTAT=I/XXDEL=D
     C*
     C                   ENDSR                                                  SRACK
