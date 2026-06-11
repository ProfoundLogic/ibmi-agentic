     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W8915R - WORK FILE BUILD - AGT COMM - WHACP
     H*****************************************************************
     H*                                                               *
     H* DESCRIPTION:                                                  *
     H*                                                               *
     H*   DATA WAREHOUSE - AGT COMM FILE                              *
     H*                                                               *
     H* CREATED: 02/06/01  C:61                                       *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*  07/22/05       C: 58      Project: 135WEH
     H*        For extracted policy#, use WMRPP/carrier number:
     H*          for: client 135;
     H*                                                               *
     H* 03/10/04 C: 58  CI015
     H*             Add start fund year PARM; extract only therefrom.
     H*                                                               *
     H* 10/09/03 C:132 PROJECT:  CI010                                *
     H*                - USE PARM DATE FOR ASOF DATE                  *
     H*                                                               *
     H* 10/02/01  C:132 PROJECT WC924                                 *
     H*               - Change: Add a check new FNCICF flag in WMFNP  *
     H*                 for a 'Y' to include in Extract = Else Bypass *
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
      *
      * AGT COMM WORK FILE
     FWFACP     IF   E           K DISK
      *
      * FUND
     FWMFNL     IF   E           K DISK
      *
      * Output - Data Warehouse - WHARP
     FWHACP     IF A E             DISK
     F                                     RENAME(PAC01:PAC02)
      *
      * AGT ASSIGNMENT FILE
     FWMAAL1    IF   E           K DISK
      *
     F**** Begin Add ****  07/22/05  **************************************
     F*   Employer Carrier File
     FWMRPL     IF   E           K DISK
      *
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D                 DS
     D  X1DATE                 1      8
     D  X1MM                   1      2
     D  X1DD                   3      4
     D  X1YY                   5      8
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     F**** End Add ******  07/22/05  **************************************
      *
      *

     IPAC02
     I              ACPOL#                      A2POL#
     I              ACAGT#                      A2AGT#
     I              ACPROD                      A2PROD
     I              ACDIV                       A2DIV
     I              ACTRTP                      A2TRTP
     I              ACTYDS                      A2TYDS
     I              ACAMT                       A2AMT
     I              ACTNDT                      A2TNDT
     I              ACCHCK                      A2CHCK
     I              ACFND                       A2FND
     I              ACPSDT                      A2PSDT
     I              ACTOCL                      A2TOCL
     I              ACRATE                      A2RATE
     I              ACKEY                       A2KEY
     I              ACAODT                      A2AODT
      *
      ************************************************************
      *
      *  KLIST
      *
      *  Fund
      *

     C     FNKY          KLIST
     C                   KFLD                    ACCO#
     C                   KFLD                    ACFND
      *
     C     AAKY          KLIST
     C                   KFLD                    ACCO#
     C                   KFLD                    ACFND
     C                   KFLD                    ACAGT#
      *
     C**** Begin Add ****  07/22/05  **************************************
     C     RPKEY         KLIST
     C                   KFLD                    ACCO#
     C                   KFLD                    ACFND
     C                   KFLD                    ACEMP#
     C                   KFLD                    ACDIV
     C                   KFLD                    ACFYR
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C**** End Add ******  07/22/05  **************************************
      *
      ************************************************************
      *
      *   *LIKE defines
      *
      *
      ************************************************************
      *
      *  PLIST definitions
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@RNDT            8
     C***** BEG ADD ******* 03/10/04 **********************************
     C                   PARM                    @@STYR            3 0
     C***** END ADD ******* 03/10/04 **********************************
      *
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
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DT1           PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DT2           PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *
      ************************************************************
      *  Begin
      ************************************************************
      *
      *  INITIALIZE
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVEL     @@RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      *OFF
      *
     C                   MOVEL     @@RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT            7 0
      *
     C                   ELSE
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
      *
     C                   ENDIF                                                  SRERR
      *
      *  READ WORK FILE
      *
     C                   READ      PAC01                                  60
     C     *IN60         DOWEQ     '0'
      *
      *   Selection: bypass all test groups.
      *
     C     ACCO#         IFEQ      *ZERO
     C                   GOTO      NEXT
     C                   ENDIF
      *
     C     FNKY          CHAIN     PFN01                              60
      *
     C     FNTEST        IFEQ      'Y'
     C     *IN60         OREQ      *ON
     C**** BEGIN ADD **** 10/02/01 *************************************
     C     FNCICF        ORNE      'Y'
     C**** END ADD ****** 10/02/01 *************************************
     C                   GOTO      NEXT
     C                   ENDIF
      *
      * WRITE TO OUTPUT
      *
     C***** BEG ADD ******* 03/10/04 **********************************
     C     ACFYR         IFLT      @@STYR
     C                   GOTO      NEXT
     C                   ENDIF
     C***** END ADD ******* 03/10/04 **********************************
      *
     C                   EXSR      SROUT
      *
     C     NEXT          TAG
     C                   READ      PAC01                                  60
     C                   ENDDO
      *
      *
     C                   MOVE      '1'           *INLR
      *
      *
      *
      *================================================================
      * SROUT - Write record to output file WHACP
      *================================================================
     C     SROUT         BEGSR
      *
     C  N60
     CAN 60              READ      PAC02                                  60
     C                   CLEAR                   PAC02
      *
      *  Policy Number
     C**** Begin Add ****  07/22/05  **************************************
     C     CLTID         IFEQ      '135'
     C     RPKEY         CHAIN     PRP01                              62
     C     *IN62         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANK
     C                   MOVEL(P)  RPCAP#        A2POL#
     C                   ELSE
     C**** End Add ******  07/22/05  **************************************
      *
     C                   MOVEL(P)  ACFND         A2POL#
     C                   MOVEL(P)  ACEMP#        XXTMP             9
     C                   CAT(P)    XXTMP:0       A2POL#
     C                   MOVEL(P)  ACFYR         XXTMP             9
     C                   CAT(P)    XXTMP:0       A2POL#
     C**** Begin Add ****  07/22/05  **************************************
     C                   ENDIF                                                  e 62 OFF...&
     C                   ENDIF                                                  e CLTID=135
     C**** End Add ******  07/22/05  **************************************
      *
      *  AGENT
      *  AGENT
      *
     C     AAKY          CHAIN     PAA01                              65
     C     *IN65         IFEQ      '0'
     C                   MOVEL(P)  AAFEIN        A2AGT#
     C                   MOVEL(P)  AAMOD         XXMOD             3
     C     A2AGT#        CAT(P)    XXMOD:0       A2AGT#
     C                   ELSE
     C                   MOVE      *BLANKS       A2AGT#
     C                   END
      *
     C**************       MOVELACFND     A2AGT#    P
     C**************       MOVELACAGT#    XXTMP   9 P
     C**************       CAT  XXTMP:0   A2AGT#    P
      *
     C                   MOVEL     ACPROD        A2PROD
     C                   Z-ADD     ACDIV         A2DIV
      *
     C     ACCOTP        IFEQ      'E'
     C     ACCOTP        OREQ      'A'
     C                   MOVE      'E'           A2TRTP
     C                   ENDIF
      *
     C     ACCOTP        IFEQ      'P'
     C     ACCOTP        OREQ      'V'
     C                   MOVE      'P'           A2TRTP
     C                   ENDIF
      *
     C                   MOVEL(P)  ACTYDS        A2TYDS
     C                   Z-ADD     ACAMT         A2AMT
      *
     C     ACCOTP        IFEQ      'V'
     C                   MULT      -1            A2AMT
     C                   ENDIF
      *
     C                   MOVEL     ACTNDT        X1DATE
     C                   EXSR      GETDAT
     C                   MOVEL     X2DATE        A2TNDT
      *
     C                   MOVEL(P)  ACCHCK        A2CHCK
     C                   Z-ADD     ACFND         A2FND
      *
     C                   MOVEL     ACPSDT        X1DATE
     C                   EXSR      GETDAT
     C                   MOVEL     X2DATE        A2PSDT
      *
     C                   Z-ADD     ACTOCL        A2TOCL
     C                   Z-ADD     ACRATE        A2RATE
      *
      *  UNIQUE KEY
      *
     C                   MOVEL(P)  ACCO#         A2KEY
     C                   MOVEL(P)  ACFND         XXTMP             9
     C                   CAT(P)    XXTMP:0       A2KEY
     C                   MOVEL(P)  ACAGT#        XXTMP             9
     C                   CAT(P)    XXTMP:0       A2KEY
     C                   MOVEL(P)  ACEMP#        XXTMP             9
     C                   CAT(P)    XXTMP:0       A2KEY
     C                   MOVEL(P)  ACDIV         XXTMP             9
     C                   CAT(P)    XXTMP:0       A2KEY
     C                   MOVEL(P)  ACFYR         XXTMP             9
     C                   CAT(P)    XXTMP:0       A2KEY
      *
     C                   MOVEL     ACPSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXTMP
     C                   CAT(P)    XXTMP:0       A2KEY
      *
     C                   MOVEL     ACTNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXTMP
     C                   CAT(P)    XXTMP:0       A2KEY
      *
      *  ZZZ
      *
      ***** BEG CHG **** 10/09/03 ********************************
      * Change the Parm Date into the AS OF Date for output record
     C                   MOVEL     @@RNDT        ZZRNDT            8 0
     C                   MOVEL     ZZRNDT        X1DATE
     C**************       MOVELDSDATE    X1DATE
      ***** END CHG **** 10/09/03 ********************************
     C                   EXSR      GETDAT
     C                   MOVEL     X2DATE        A2AODT
      *
      *  Write to output file
      *
     C                   WRITE     PAC02
     C                   ENDSR                                                  SROUT
      *
      *
      *
      *================================================================
      * GETDAT- Return date formatted mm/dd/yy
      *================================================================
     C     GETDAT        BEGSR
      *
     C     X1MM          CAT(P)    '/':0         X2DATE           10
     C                   CAT(P)    X1DD:0        X2DATE
     C                   CAT(P)    '/':0         X2DATE
     C                   CAT(P)    X1YY:0        X2DATE
      *
     C                   ENDSR                                                  GETDAT
      *
      *
      *
