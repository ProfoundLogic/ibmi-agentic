     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/define CONVERT
     H/TITLE W8701R - AUTO UPDATE QUOTE TYPE TO 'LOST'
     H***************************************************************
     H*
     H*   PROGRAM DESCRIPTION:
     H*      The daily batch will automatically update the quote
     H*      tracking type to 'L' (lost) if a quoted ('Q') or
     H*      pended ('P') application is not written within a specified
     H*      period of time after the estimated effective date.
     H*      The length of time (# of days) is identified at the
     H*      group master level.
     H*
     H*   CALLED FROM:
     H*      W6000C - DAILY BATCH
     H*
     H*   CALLS TO:
     H*      NONE
     H*
     H*   CREATED:  07/20/00   C:61   PROJECT: WC915
     H***************************************************************
     H*   REVISIONS:
      *
      * 06/22/20 C:33  Project: SY406
      *                Update EMFQLK based on the quote status change *
     H*
     H*  11/27/12   C:283    SUPPORT: FQ093 81138
     H*                      Correct the process to search record in
     H*                      WMEMP file after Reading file WAQTP.
     H*                      WMEMP should not be looped.
     H*
     H*  01/08/10   C:234    SUPPORT: 71017 WA146
     H*                      Correction to 10/26/09 changes. Auto
     H*                      update quote to lost status not
     H*                      working correctly.
     H*                      Get correct Quote libs for WMEML fetch.
     H*
     H*  10/26/09   C:180    Omit changing quote status to lost when
     H*                      employer is flagged as 'Renew to Quote'
     H*
     H*
     H*  7/06/05    C:58     Project: WC1310
     H*               Added code to set new Auto Lost flag, without
     H*                 overwriting comments.
     H*
     H***************************************************************
     H*
     H*   INDICATOR USAGE:
     H*     INDICATOR   FUNCTION DESCIPTION
     H*
     H*        60       GENERAL
     H*
     H***************************************************************
      *
      *  Group Master
     FWMFNL     IF   E           K DISK
      *  Employer Master
     FWMEML     IF   E           K DISK    USROPN
      *
      *  Agent Quote
     FWAQTL     UF   E           K DISK
      *
      *

     D QCA             S             55    DIM(26) CTDATA PERRCD(1)             CMD EXEC/QCAEXEC
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
      *
     D XXMSG1          C                   CONST('Auto update to lost')
     D GETLIB          C                   CONST('RTVQTELIB')
      *
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)

       //***  Begin Add  ***  06/22/20  ***************
      /copy wPrEmplr
       //***  End   Add  ***  06/22/20  ***************
      *
      *
      *  Group Master
      *

     C     FNKY          KLIST
     C                   KFLD                    XXCO#
      *
      *  Quote Master
      *
     C     QTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
      *
      *  Employer master
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C                   KFLD                    QTEMP#
     C                   KFLD                    QTDIV
      *
      *  *LIKE DEFINE
      *
     C     *LIKE         DEFINE    QTCO#         XXCO#
     C     *LIKE         DEFINE    QTEFDT        XXDATE
     C     *LIKE         DEFINE    QTEFDT        XXLTDT
      *
      *  PLIST
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *
     C                   MOVE      QQCO#         XXCO#
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
      *
      *  While have Group master record AND
      *  # of days beyond effect date before considered Lost entered
      *
     C     FNKY          CHAIN     PFN01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     FNLOST        IFNE      *ZERO
     C                   EXSR      UPDQTE
     C                   ENDIF                                                  FNLOST NE 0
      *
     C     FNKY          READE     PFN01                                  60
     C                   ENDDO                                                  *IN60 FNLOST
      *
     C                   MOVE      '1'           *INLR
      *
      *
      *
      *==============================================================
      *  UPDQTE - UPDATE AGENT QUOTE RECORDS TO 'LOST' AS REQUIRED
      *==============================================================
     C     UPDQTE        BEGSR
      * Use PDSQTE/WMEML's EMRQFL when running on client machine.
      * When running on PDSDEV use libl WMEML's EMRQFL.
     C                   MOVE      *BLANKS       LIBNAM           10
     C                   MOVE      *BLANKS       CMDST1           55
     C                   MOVE      *BLANKS       PDS               1
     C                   CALL      'SACHKPDS'
     C                   PARM                    PDS               1
     C     PDS           IFEQ      *OFF
     C                   CALL      GETLIB
     C                   PARM      '*QTE'        ENV               4
     C                   PARM                    LIBNAM
     C     LIBNAM        IFNE      *BLANKS
     C                   MOVEL(P)  QCA(1)        CMDST1
     C                   CAT(P)    LIBNAM:0      CMDST1
     C                   CAT(P)    QCA(2):0      CMDST1
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDST1
     C                   PARM                    LENGTH           15 5
     C                   OPEN      WMEML
     C*
      *  READ Agent quote file records that are Pended or Quoted
      *
     C     QTKY          CHAIN     PQT01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     QTTYPE        IFEQ      'P'
     C     QTTYPE        OREQ      'Q'
      *
     C     EMKY          CHAIN     PEM01                              61
      *
     C                   EXSR      VALUPD
      *
     C                   ENDIF                                                  QTTYPE = P Q
      *
      *   READ NEXT QUOTE RECORD
      *
     C     QTKY          READE     PQT01                                  60
     C                   ENDDO                                                  *IN60
     C*
     C                   MOVEL(P)  QCA(3)        CMDST1
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDST1
     C                   PARM                    LENGTH
     C                   CLOSE     WMEML
     C                   ENDIF                                                  Libnam <> ' '
     C*
     C                   ELSE
     C                   OPEN      WMEML
      *
      *  READ Agent quote file records that are Pended or Quoted
      *
     C     QTKY          CHAIN     PQT01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     QTTYPE        IFEQ      'P'
     C     QTTYPE        OREQ      'Q'
      *
     C     EMKY          CHAIN     PEM01                              61
      *
     C                   EXSR      VALUPD
      *
     C                   ENDIF                                                  QTTYPE = P Q
      *
      *   READ NEXT QUOTE RECORD
      *
     C     QTKY          READE     PQT01                                  60
     C                   ENDDO                                                  *IN60
     C                   CLOSE     WMEML
     C*
     C                   ENDIF                                                  Pds if *off
      *
     C                   ENDSR                                                  UPDQTE
      *
      *==============================================================
      *  VALUPD - Validate and Update the Quote to 'L'.
      *==============================================================
     C     VALUPD        BEGSR
      *
     C  N61EMRQFL        COMP      'Y'                                    61
     C     *IN61         IFEQ      *OFF
      *
      *    Get date beyond which account is lost
      *
     C                   Z-ADD     QTEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##DATE            8 0
      *
     C                   Z-ADD     FNLOST        ##DAYS            5 0
      *
     C                   CALL      'SRFUTR8'
     C                   PARM                    ##DATE
     C                   PARM                    ##DAYS
     C                   PARM      *BLANK        ##ERR             1
      *
     C                   Z-ADD     ##DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXLTDT
      *
      *  IF Lost date < SYSM DT, UPDATE QUOTE TYPE TO 'L' LOST
      *
     C     XXLTDT        IFLT      XXDATE
     C     ##ERR         ANDEQ     *OFF
     C     SRERR         ANDEQ     *OFF
      *
     C                   MOVE      'L'           QTTYPE
     C                   Z-ADD     XXDATE        QTWKDT
     C                   MOVE      'Y'           QTAULS
     C                   MOVEL(P)  Q1USER        QTUSER
     C                   MOVEL(P)  Q1PGM         QTPGM
     C                   Z-ADD     DSTIME        QTTIME
     C                   Z-ADD     XXDATE        QTDT
      *
     C                   UPDATE    PQT01
       //***  Begin Add  ***  06/22/20  ***************
      /free
          UpdFQLockSts(QTco#:QTfnd:QTemp#:QTdiv:QTtype);
      /end-free
       //***  End   Add  ***  06/22/20  ***************
      *
     C                   ENDIF                                                  EMRQFL=Y
      *
     C                   ENDIF                                                  LTDT < DATE
      *
     C                   ENDSR
      *
      *
**  QCAEXEC TABLE
OVRDBF FILE(WMEML) TOFILE(
/WMEML) MBR(*FIRST)
DLTOVR FILE(WMEML)
