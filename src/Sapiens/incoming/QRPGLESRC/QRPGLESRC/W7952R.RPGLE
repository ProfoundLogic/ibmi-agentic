     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7952R - I/M/R STATUS REPORT - BUILD WORKFILE
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM BUILDS THE WORKFILE USED FOR THE REPORT PROGRAM. *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W7951C                                                      *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H* PROGRAMMER: LARRY KORZON                                      *
     H* CREATED: 05/26/92               PROJECT: NW006                *
     H*                                                               *
     H* INDICATORS:                                                   *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  05/25/07 C:191 PROJECT: WC1515                               *
     H*                 Check claims authority                        *
     H*                 for no-auth, do not include claimant info     *
     H*                                                               *
     H*  09/24/92 KWK  PROJECT NW006                                  *
     H*                THIS CODE NEEDS TO BE CONSISTENT WITH W6029R   *
     H*                TO LOOK AT DATE REPORTED (CMDTRP) NOT DATE     *
     H*                ENTERED (CMENTD) WHEN CONSIDERING CLAIMS       *
     H*                THAT FALL WITHIN A DATE RANGE.                 *
     H*                                                               *
     H*  10/11/93 C:09 PER REQUEST FROM PAM MYERS, DO NOT SHOW        *
     H*                CLAIMS IF THE DATE REPORTED IS LESS THAN       *
     H*                07/01/93.                                      *
     H*                                                               *
     H*  05/25/95 C:33 PROJECT# 25WAR                                 *
     H*                USE CMENTD, NOT CMDTRP TO RECOGNIZE NEW CLAIMS *
     H*****************************************************************
     FWXSTL1    UF A E           K DISK
     FWMCML     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FWMTHL1    IF   E           K DISK
     FWMTHL     IF   E           K DISK
     F                                     RENAME(PTH01:PTH02)
     FWMEML     IF   E           K DISK
     FWMCDL     IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** BEG ADD ****** 05/25/07 **********************************
     D XXATTX          C                   CONST('*** NO AUTHORITY ***')
      *** END ADD ****** 05/25/07 **********************************

     C*
     C*   KEY LIST FOR THE STATUS WORKFILE
     C*
     C     STKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    THFYR
     C                   KFLD                    THCASE
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFRDT
     C*
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     CMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    THFYR
     C                   KFLD                    THCASE
     C*
     C     CMKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  KEY LIST FOR THE TRANSFER HISTORY FILE
     C*
     C     THKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXDATE
     C*
     C     THKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     THKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     THKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXDATE
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*   KEY LIST FOR CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
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
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*
     C*  INITIALIZE ALL PROGRAM DEFINED VARIABLES
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMACDT        XXFRDT
     C     *LIKE         DEFINE    CMACDT        XXTODT
     C     *LIKE         DEFINE    CMACDT        XXDATE
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFRDT        XQFRDT            8 0
     C                   MOVE      QQTODT        XQTODT            8 0
     C*
     C                   Z-ADD     XQFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     XQTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      '0'           *IN70
     C     XXEMP#        IFEQ      0
     C                   MOVE      '1'           *IN70
     C                   END
     C                   Z-ADD     *ZEROS        X                 3 0
     C******
     C*  GET NEW CLAIMS FROM CLAIM MASTER
     C******
     C   70CMKY1         CHAIN     PCM01                              50
     C  N70CMKY3         CHAIN     PCM01                              50
     C     *IN50         DOWEQ     '0'
     C     CMENTD        IFGE      XXFRDT
     C     CMENTD        ANDLE     XXTODT
     C     CMENTD        ANDGE     0930701
     C*
     C*  GET LOST TIME FLAG AS OF THE 'TO DATE'
     C*
     C                   Z-ADD     XXTODT        XXDATE
     C                   Z-ADD     CMFYR         XXFYR
     C                   MOVE      CMCASE        XXCASE
     C     THKY3         SETGT     PTH02
     C     THKY2         READPE    PTH02                                  60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      THTO          STCUTY
     C*
     C                   END
     C*
     C*  GET EMPLOYER NAME AND CLAIMANT NAME
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLNAM           56
     C                   MOVEL     CDFSN         QQFNAM           15
     C                   MOVEL     ' '           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLNAM
     C                   PARM                    QQFNAM
     C                   PARM                    QQTERM
     C                   MOVEL     QQLNAM        XXNAME           56
     C                   END
     C*
     C*  LOAD FIELDS FOR WORKFILE
     C*
     C                   Z-ADD     CMCO#         STCO#
     C                   Z-ADD     CMFND         STFND
     C                   Z-ADD     CMEMP#        STEMP#
     C                   Z-ADD     CMDIV         STDIV
     C                   MOVEL     EMNAME        STENAM
     C                   Z-ADD     CMFYR         STFYR
     C                   MOVEL     CMCASE        STCASE
     C                   MOVEL     XXNAME        STCNAM
     C                   MOVE      '1'           STTYPE
     C                   MOVE      *BLANKS       STPRTY
     C                   Z-ADD     0             STPRDT
     C                   Z-ADD     CMENTD        STCUDT
     C                   MOVEL     Q1USER        STUSER
     C                   MOVEL     Q1PGM         STPGM
     C                   Z-ADD     DSTIME        STTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         STDT
     C*
     C                   WRITE     PST01
     C*
     C                   ENDIF                                                  CMENTD >= XXFRDT
     C*                                                    CMENTD <= XXTODT
     C*
     C   70CMKY1         READE     PCM01                                  50
     C  N70CMKY3         READE     PCM01                                  50
     C                   END
     C*******
     C*  GET OLD CLAIMS WHOSE STATUS HAS CHANGED
     C*******
     C                   Z-ADD     XXFRDT        XXDATE
     C     THKEY         SETLL     PTH01
     C     THKY1         READE     PTH01                                  51
     C     *IN51         DOWEQ     '0'
     C     THTRDT        ANDLE     XXTODT
     C*
     C*  CHECK SELECTION CRITERIA
     C*
     C     *IN70         IFEQ      '0'
     C     CMKY2         CHAIN     PCM02                              60
     C     CMEMP#        IFNE      XXEMP#
     C                   GOTO      NEXT
     C                   END
     C                   END
     C*
     C*  CHECK WORK FILE TO PREVENT DUPLICATE RECORDS
     C*
     C     THFROM        IFNE      *BLANKS
     C     STKEY         CHAIN     PST01                              50
     C     *IN50         IFEQ      '1'
     C*
     C*  GET PRIOR AND CURRENT TYPE AND DATE
     C*
     C                   Z-ADD     THFYR         XXFYR
     C                   MOVE      THCASE        XXCASE
     C*
     C     THKY2         CHAIN     PTH02                              52
     C*
     C     *IN52         DOWEQ     '0'
     C     THTRDT        ANDLE     XXTODT
     C*
     C     THTRDT        IFLT      XXFRDT
     C*
     C                   MOVE      THTO          STPRTY
     C                   Z-ADD     THTRDT        STPRDT
     C*
     C                   ELSE
     C*
     C                   MOVE      THTO          STCUTY
     C                   Z-ADD     THTRDT        STCUDT
     C*
     C                   ENDIF                                                  THTRDT
     C*
     C     THKY2         READE     PTH02                                  52
     C*
     C                   ENDDO                                                  *IN52
     C*
     C     CMKY2         CHAIN     PCM02                              60
     C*
     C     STPRTY        IFNE      STCUTY
     C     CMENTD        ANDGE     0930701
     C*
     C*  GET EMPLOYER NUMBER AND NAME, AND CLAIMANT NAME
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLNAM           56
     C                   MOVEL     CDFSN         QQFNAM           15
     C                   MOVEL     ' '           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLNAM
     C                   PARM                    QQFNAM
     C                   PARM                    QQTERM
     C                   MOVEL     QQLNAM        XXNAME           56
     C                   END
     C*
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *BLANKS       XXNAME
     C                   MOVEL     XXATTX        XXNAME
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C*  LOAD FIELDS FOR WORKFILE
     C*
     C                   Z-ADD     CMCO#         STCO#
     C                   Z-ADD     CMFND         STFND
     C                   Z-ADD     CMEMP#        STEMP#
     C                   Z-ADD     CMDIV         STDIV
     C                   MOVEL     EMNAME        STENAM
     C                   Z-ADD     CMFYR         STFYR
     C                   MOVEL     CMCASE        STCASE
     C                   MOVEL     XXNAME        STCNAM
     C                   MOVE      '2'           STTYPE
     C                   MOVEL     Q1USER        STUSER
     C                   MOVEL     Q1PGM         STPGM
     C                   Z-ADD     DSTIME        STTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         STDT
     C*
     C                   WRITE     PST01
     C*
     C                   END
     C                   END
     C                   END
     C     NEXT          TAG
     C*
     C     THKY1         READE     PTH01                                  51
     C                   END
     C*
     C*  END OF PROGRAM
     C*
     C                   MOVE      '1'           *INLR
     C*
