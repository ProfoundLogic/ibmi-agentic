     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H* ***************************************************************
     H/TITLE W0070R1B - SUSPENSION INFORMATION.
     H* ***************************************************************
     H* DESCRIPTION:  THIS PROGRAM ALLOWS THE USER TO INSERT/UPDATE   *
     H*               SUSPENSION INFORMATION.                         *
     H*                                                               *
     H* CALLED FROM:  W0080R1  via F16                                *
     H* CALLS TO:     NONE                                            *
     H* CREATED BY:   235                                             *
     H* PROJECT:      139WNF               03/23/10                   *
     H* ***************************************************************
    H*****************************************************************
     * REVISIONS:                                                    *
      *                                                               *
      * 06/09/20 C:318 Project: SY420
      *                Mask SSN
      *
      * 11/15/10  C:189 Support wc139 74686                           *
      *                 pressing enter on the screen has no effect    *
    H*****************************************************************
     H*                                                               *
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW0070F1B  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMCML1    UF A E           K DISK
     F                                     INFDS(DBFDS)
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     D PRDS            S             40    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      *
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D                 DS
     D  XXSUSN                 1    150
     D  SCSUS1                 1     50
     D  SCSUS2                51    100
     D  SCSUS3               101    150
      *
     D MODEC           C                   CONST('- Change -')
     D MODED           C                   CONST('-Display- ')
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDS           DS
     D  DBRRN                397    400B 0
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C*
     C* *****************************************************************
     C*  *ENTRY PARAMETERS FOR PROGRAM
     C* *****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFND             3 0
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQRTCD            1
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
     C* *****************************************************************
     C*  *PROCESSING BEGINS HERE
     C* *****************************************************************
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVEL(P)  MODEC         SCMODE
     C     QQCALL        COMP      '1'                                    09
     C                   MOVE      PRDS(1)       SCPRDS
     C   09              MOVEL(P)  MODED         SCMODE
     C*
     C*  RETRIEVE THE USER PARAMETER, COMPANY AND FUND RECORDS
     C*
     C                   Z-ADD     0             XXCO#
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     UPCO#         XXCO#
     C                   ENDIF
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     XXFND         SCFND
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C*  RETRIEVE THE CLAIMANT INFORMATION
     C*
      *  FYR-CASE
     C                   MOVE(P)   XXFYR         XAFYR             3
     C                   MOVE(P)   XXCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCL
     C     SCYRCL        CAT       XACASE:0      SCYRCL
      *
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     DBRRN         XXRRN             9 0
     C                   ENDIF
     C*
      *  CLAIMANT SS#
     C     CMSS#         IFNE      *BLANK
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C     SS1           CAT(P)    '-':0         SCSS#
     C     SCSS#         CAT       SS2:0         SCSS#
     C     SCSS#         CAT       '-':0         SCSS#
     C     SCSS#         CAT       SS3:0         SCSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   ENDIF
      *
      *
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'
     C                   PARM                    CMEMP#
     C                   PARM                    CMDIV
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    XXEDN            80
     C                   ENDIF
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   MOVE      *BLANK        SCSCL#
     C                   MOVEL     CMACL#        SCSCL#
     C*
     C*  RETRIEVE THE CLAIMANT NAME
     C*
     C     CDKY          CHAIN     PCD01                              60
     C                   MOVE      *BLANKS       SCCNAM
     C                   MOVEL     CDLSN         SCCNAM
     C     CDLSN         IFNE      *BLANKS
     C     SCCNAM        CAT       ',':0         SCCNAM
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C                   ELSE
     C                   MOVEL     CDFSN         SCCNAM
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C                   ENDIF
     C*
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   EXSR      LOAD
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C                   DOW       *INKC = *OFF and *INKL = *OFF
     C                   EXFMT     SCR01
     C* *****************************************************************
     C*  Process any changes
     C* *****************************************************************
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      'B'           qqrtcd
 RXS C     *INKC         WHENEQ    *ON
     C                   MOVE      'A'           qqrtcd
     C                   OTHER
     C                   EXSR      ERRCHK
     C                   IF        SRERR <> '1'
     C                   EXSR      UPDATR
     C                   MOVE      'B'           qqrtcd
     C                   LEAVE
     C                   ENDIF
      ****** beg add ****  11/15/10 ***********************************
     C                   LEAVE
      ****** end add ***** 11/15/10 ***********************************
     C                   ENDSL
     C                   ENDDO
     C
     C                   SETON                                        LR
     C**************************************************************************
     C*  CLEAR THE SCREEN ARRAYS
     C*****************************************************************
     C     ERRCHK        BEGSR
     C* Suspension Effective date
     C     SCSUEF        IFNE      0
     C                   MOVE      SCSUEF        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSUEF
     C                   IF        SRERR = '1'
     C                   SETON                                        45
     C                   EXFMT     SCR01
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C**************************************************************************
     C*  FILL SCREEN ARRAYS
     C*****************************************************************
     C     LOAD          BEGSR
     C     CMKY          CHAIN     PCM01                              99
     C                   IF        *IN99 = *OFF
     C                   z-add     cmsuef        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         scsuef
     C                   MOVE      CMSUSN        XXSUSN
     C                   ELSE
     C                   MOVE      *BLANKS       SCSUEF
     C                   MOVE      *BLANKS       XXSUSN
     C                   ENDIF
     C                   ENDSR
     C**************************************************************
     C*  WRITER  SUBROUTINE - UPDATE RECORDS                       *
     C**************************************************************
     C     UPDATR        BEGSR
     C
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   MOVEL     DSTIME        CMTIME
     C*
     C                   z-add     scsuef        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         cmsuef
     C                   MOVE      XXSUSN        CMSUSN
     C                   UPDATE    PCM01
     C                   ENDSR
     C* ************************************************************************
** PGM DESC
         Suspension Information
