     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE F6213R - 837 FORMAT MANAGEMENT SEARCH
     H*
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*     THIS PROGRAM SEARCHES TRANSMISSION MANAGEMENT FILE.       *
     H*                                                               *
     H*  CALLED BY: F6210R - 837 FORMAT PROCESS                       *
     H*                                                               *
     H*  CALLS TO:  NONE                                              *
     H*                                                               *
     H*****************************************************************
     H*  PROGRAMMER:                                                  *
     H*  CREATED:   08/01/99           PROJECT: KY010                 *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/22/14 C:079 Project: MC376                                 *
     H*                Call F6216R passing in TsetID                  *
     H*                                                               *
     H* 04/30/14 C:296 Project: MC376                                 *
     H*                Adding option 7 for "Work With 997 Ack Errors" *
     H*                and calling F6216R.                            *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*   20       RECORDS NOT FOUND                                  *
     H*   50       SUBFILE DISPLAY                                    *
     H*   59-65    GENERAL CHAIN & READ                               *
     H*****************************************************************
     H*
     FF6213F    CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     SFILE(SCR01S:RRN)
     F                                     INFDS(FIDS01)
     FFEMFL     IF   E           K DISK
     F*** Beg Add *** 07/22/14 *********************************************
     F                                     INFDS(DBFDS)
     F*** End Add *** 07/22/14 *********************************************
     FSMCOL     IF   E           K DISK
     F*
     D***** Beg Add ***** 04/30/14 ******************************
     D  ##RRN          s                   like(rrn)
     D  ##997SgID      s              3a
     D  ##997ErCd      s              3a
     D  ##Call         s              1a
     D  ##KC           s              1a
     D  ##KL           s              1a
     D***** End Add ***** 04/30/14 ******************************

     D PRDS            S             40    DIM(1) CTDATA PERRCD(1)
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C/EJECT
     C* ============================================================
     C*        M A I N L I N E
     C* ============================================================
     C                   EXSR      INIT
     C*
     C     SCR01TAG      TAG
     C                   EXSR      SCR01T
     C*
     C                   SELECT
     C*
     C*   F12 = CANCEL
     C*
     C     *IN12         WHENEQ    *ON
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C*
     C                   ENDSL                                                  END CMD KEYS
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C     1             CHAIN     SCR01S                             50
     C     *IN50         DOWEQ     *OFF
     C*
     C     SCSEL         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      SCOTDT        QQOTDT
     C                   MOVE      SCOTTM        QQOTTM
     C                   GOTO      TERM
     C                   ENDIF
     C***** Beg Add ***** 04/30/14 ******************************
     C* Work With 997 Acknowledgement Errors
     C*
     C                   if        scsel = '7'
     C*
     C                   eval      xxCo# = scCo#
     C                   eval      xxJSt = scJSt
     C                   eval      xxOtTm = scOtTm
     C                   Z-ADD     scOtDt        SRDTI                           ACK ENTERED
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDYY--> CYMD
     C                   MOVEL(P)  SRDTO         xxOtDt
     C*
     C     mfky          Chain     femfl                              99
     C*
     C                   eval      ##rrn = DBRRN
     C                   Call      'F6216R'      AckErr
     C*
     C                   Endif
     C*
     C***** End Add ***** 04/30/14 ******************************
     C*
     C                   READC     SCR01S                                 50
     C                   ENDDO
     C*
     C*   END OF PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C/EJECT
     C* ============================================================
     C*   ROUTINE - SCR01T
     C* ============================================================
     C     SCR01T        BEGSR
     C*
     C*   Display Formats
     C*
     C     *IN12         DOUEQ     *ON
     C     SCPOS         OREQ      *ZEROS
     C*
     C                   Z-ADD     0             RRN
     C                   MOVE      *ON           *IN50
     C                   WRITE     SCR01C
     C                   MOVE      *OFF          *IN50
     C*
     C*   Retrieve the Submission File
     C*
     C     MFKY          SETLL     PMF01
     C                   EXSR      LOAD
     C*
     C     *IN59         IFEQ      *ON
     C     RRN           ANDEQ     0
     C                   ADD       1             RRN
     C                   WRITE     SCR01S                                       SFL
     C                   END
     C*
     C                   Z-ADD     *ZEROS        XXOTDT
     C                   Z-ADD     *ZEROS        XXOTTM
     C                   Z-ADD     *ZEROS        SCPOS
     C                   WRITE     SCR01                                        CMD KEYS
     C                   EXFMT     SCR01C
     C*
     C     SCPOS         IFNE      *ZEROS
     C                   MOVE      SCPOS         XXDATE                         Trans Dt
     C                   Z-ADD     XXDATE        SRDTI                           ACK ENTERED
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDYY--> CYMD
     C                   MOVEL(P)  SRDTO         XXOTDT
     C                   Z-ADD     *ZEROS        XXOTTM
     C                   END
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C* ============================================================
     C*** ROUTINE - INIT
     C* ============================================================
     C*
     C     INIT          BEGSR
     C*
     C*** Define Parameter Lists
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQJST             2
     C                   PARM                    QQTSET            3
     C                   PARM                    QQOTDT            8
     C                   PARM                    QQOTTM            6
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
     C***** Beg Chg ***** 04/30/14 ******************************
     C     AckErr        PLIST
     C                   PARM                    ##CALL
     C                   PARM                    ##KC
     C                   PARM                    ##KL
     C                   PARM                    ##RRN
     C                   PARM                    ##997SGID
     C                   PARM                    ##997ERCD
     C***** End Chg ***** 04/30/14 ******************************
     C*
     C*** Define Key Lists
     C*
     C*   SMCOL - Company Master
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   FEMFL - Format Mgt File
     C*
     C     MFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXTSET
     C                   KFLD                    XXOTDT
     C                   KFLD                    XXOTTM
     C*
     C     MFKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXTSET
     C*
     C*
     C     *LIKE         DEFINE    MFCO#         XXCO#
     C     *LIKE         DEFINE    MFJST         XXJST
     C     *LIKE         DEFINE    MFTSET        XXTSET
     C     *LIKE         DEFINE    MFOTDT        XXOTDT
     C     *LIKE         DEFINE    MFOTTM        XXOTTM
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQJST         XXJST
     C                   MOVE      QQTSET        XXTSET
     C                   MOVE      QQOTDT        XXDATE            8 0
     C                   Z-ADD     XXDATE        SRDTI                           ACK ENTERED
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDYY--> CYMD
     C                   MOVEL(P)  SRDTO         XXOTDT
     C                   MOVE      *ZEROS        XXOTTM
     C*
     C*   Retrieve Company Record
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       CONAME
     C                   END
     C*
     C*   Initialize Screen Fields
     C*
     C                   MOVE      PRDS(1)       SCPRDS
     C                   MOVEL     CONAME        SCCONM
     C                   MOVE      XXJST         SCJST
     C                   MOVE      *BLANKS       SCMSG
     C*
     C                   TIME                    DSDTTM
     C*
     C                   ENDSR
     C/EJECT
     C* ============================================================
     C*** ROUTINE - LOAD
     C* ============================================================
     C     LOAD          BEGSR
     C*
     C     *IN59         DOUEQ     *ON
     C     MFKY2         READE     PMF01                                  59
     C*
     C     *IN59         IFEQ      *OFF
     C*** Beg Add *** 07/22/14 ********************************************
     C                   eval      scCo# = MFCo#
     C*** End Add *** 07/22/14 ********************************************
     C                   Z-ADD     MFOTDT        SRDTI                           ORIG TRANS
     C                   CALL      'SRCVTDAT'    DTMDYY                         CYMD--> MDYY
     C                   MOVEL(P)  SRDTO         SCOTDT
     C                   MOVE      MFOTTM        SCOTTM                         TRANSMSN TM
     C                   Z-ADD     MFLTDT        SRDTI                           LAST TRANS
     C                   CALL      'SRCVTDAT'    DTMDYY                         CYMD--> MDYY
     C                   MOVEL(P)  SRDTO         SCLTDT
     C                   Z-ADD     MFARCV        SRDTI                          ACK RECEIVED
     C                   CALL      'SRCVTDAT'    DTMDYY                         CYMD--> MDYY
     C                   MOVEL(P)  SRDTO         SCARCV
     C                   Z-ADD     MFAENT        SRDTI                          ACK ENTERED
     C                   CALL      'SRCVTDAT'    DTMDYY                         CYMD--> MDYY
     C                   MOVEL(P)  SRDTO         SCAENT
     C                   MOVEL     MFTSET        SCTSET                         MTC
     C                   ADD       1             RRN
     C                   WRITE     SCR01S
     C                   END
     C                   ENDDO
     C*
     C                   ENDSR
     C* ************************************************************************
** PGM DESC
  Format Management Search
