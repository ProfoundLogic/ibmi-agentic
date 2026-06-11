     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
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
     H*                                                               *
     H* 01/04/11  C:191  Project: WC1856 PRISS #10                    *
     H*                  Make processing backward compatible          *
     H*                                                               *
     H* 11/25/10  C:220  Project: WC1856                              *
     H*                  Implement WA726R API to retrieve policy      *
     H*                  start policy year of the group.              *
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
     H* 09/25/01 C:132 PROJECT WC924                                  *
     H*    Changed to Use input WMACL5 & Output WHXAP file Record for *
     H*    Each Group and CO#                                         *
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
      *
      * AGT COMM WORK FILE
     F***** BEGIN CHG ***** 09/25/01 **********************************
     F*****WMACP   IF  E           K        DISK
     FWMACL5    IF   E           K DISK
     F*
     F*  Agent Extract Control File
     FWHXAP     UF A E           K DISK
     F*
     F*  Extract Control File
     FWHXCP     UF   E           K DISK
     F*
     F***** END CHG ******* 09/25/01 **********************************
      * AGT ASSIGNMENT FILE
     FWMAAL1    IF   E           K DISK
      *
      * FUND
     FWMFNL     IF   E           K DISK
      *
      * Output - Data Warehouse - WHARP
     FWHACP     IF A E             DISK
     F                                     RENAME(PAC01:PAC02)
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
      *
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

     C************************************************************
     C*  KLISTS
     C************************************************************
      *
      *  Fund
      *
     C     FNKY          KLIST
     C                   KFLD                    ACCO#
     C                   KFLD                    ACFND
      *
      *  Agent Assignment File
      *
     C     AAKY          KLIST
     C                   KFLD                    ACCO#
     C                   KFLD                    ACFND
     C                   KFLD                    ACAGT#
      *
     C***** BEGIN ADD ***** 09/25/01 **********************************
      *  CICenter Extract Control
     C     XCKY          KLIST
     C                   KFLD                    SVCO#
      *
      *  Agent Extract Control
      *
     C     XAKY          KLIST
     C                   KFLD                    SVCO#
     C                   KFLD                    SVFND
     C                   KFLD                    XXPRDT
     C***** END ADD ******* 09/25/01 **********************************
      *
      ************************************************************
      *
      *   *LIKE defines
      *
     C***** BEGIN ADD ***** 09/25/01 **********************************
     C     *LIKE         DEFINE    ACCO#         SVCO#
     C     *LIKE         DEFINE    ACFND         SVFND
     C***** END ADD ******* 09/25/01 **********************************
      *
      *
      ************************************************************
      *  PLIST definitions
      ************************************************************
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
     C*
     C     DATX          PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C************************************************************
     C*  Begin
     C************************************************************
      *
      *  INITIALIZE
      *
     C                   TIME                    DSDTTM
      *
     C*** BEGIN ADD ****** 01/04/11 ************************************
     C* Check if WA726R exists in environment
     C                   MOVEL(P)  'WA726R'      XXPGM
     C                   MOVEL     '*PGM'        XXTYP             7
     C*
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    XXPGM            10
     C                   PARM                    XXTYP
     C                   PARM                    XXOLDF            1
     C* If XXOLDF = '1' then use old process
     C* If XXOLDF = '0' then use new API WA726R
     C*** END   ADD ****** 01/04/11 ************************************
     C***** BEG ADD ******* 11/25/10 **********************************
     C                   MOVEL     @@STYR        @@YRS             3
     C***** END ADD ******* 11/25/10 **********************************
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
     C***** BEGIN ADD ***** 09/25/01 **********************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPRDT            7 0
     C***** END ADD ******* 09/25/01 **********************************
      *
      *  READ WORK FILE
      *
     C                   READ      PAC01                                  60
     C     *IN60         DOWEQ     '0'
      *
     C***** BEG ADD ******* 11/25/10 **********************************
     C*** BEGIN ADD ****** 01/04/11 ************************************
     C* Process using API only if exists in environment. Else assume old process
     C     XXOLDF        IFEQ      '0'
     C*** END   ADD ****** 01/04/11 ************************************
     C*
     C* Retrieve start policy year based on extarct start date.
     C*
     C                   MOVEL     *ZEROS        @@STYR
     C                   MOVEL     *BLANKS       XXSTYR
     C                   MOVEL     ACCO#         X1CO#             3
     C                   MOVEL     ACFND         X1FND             3
     C*
     C                   CALL      'WA726R'
     C                   PARM                    X1CO#
     C                   PARM                    X1FND
     C                   PARM                    @@YRS
     C                   PARM                    XXSTYR            3
     C                   PARM                    @@RTCD            1
     C*
     C     @@RTCD        IFEQ      'Y'
     C*
     C                   MOVEL     XXSTYR        @@STYR            3 0
     C                   ELSE
     C*
     C                   MOVEL     000           @@STYR
     C                   ENDIF
     C*
     C*** BEGIN ADD ****** 01/04/11 ************************************
     C                   ENDIF                                                  XXOLDF='0'
     C*** END   ADD ****** 01/04/11 ************************************
     C***** END ADD ******* 11/25/10 **********************************
     C*
      *   Selection: bypass all test groups.
      *
     C     ACCO#         IFEQ      *ZERO
     C                   GOTO      NEXT
     C                   ENDIF
      *
     C***** BEGIN CHG ***** 09/25/01 **********************************
      *
     C     ACCO#         IFNE      SVCO#
     C     ACFND         ORNE      SVFND
     C                   EXSR      SRXAP
     C     FNKY          CHAIN     PFN01                              60
     C                   ENDIF
      *
     C********   FNKY      CHAINPFN01                60
     C***** END CHG ******* 09/25/01 **********************************
      *
     C     FNTEST        IFEQ      'Y'
     C     *IN60         OREQ      *ON
     C**** BEGIN ADD **** 10/02/01 *************************************
     C     FNCICF        ORNE      'Y'
     C**** END ADD ****** 10/02/01 *************************************
     C                   GOTO      NEXT
     C                   ENDIF
      *
      *   Selection: date LE run date.
      *
     C     ACTRDT        IFLE      XXRNDT
     C***** BEG ADD ******* 03/10/04 **********************************
     C     ACFYR         ANDGE     @@STYR
     C***** END ADD ******* 03/10/04 **********************************
      *
      *
      * WRITE TO OUTPUT
      *
     C                   EXSR      SROUT
      *
     C                   ENDIF                                                  e TRDT <= RN
      *
     C     NEXT          TAG
     C                   READ      PAC01                                  60
     C                   ENDDO
      *
      *
     C                   MOVE      '1'           *INLR
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
      *
     C                   MOVEL(P)  ACFND         A2POL#
     C                   MOVEL(P)  ACEMP#        XXTMP             9
     C                   CAT(P)    XXTMP:0       A2POL#
     C                   MOVEL(P)  ACFYR         XXTMP             9
     C                   CAT(P)    XXTMP:0       A2POL#
      *
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
     C                   MOVEL     ACPROD        A2PROD
     C                   Z-ADD     ACDIV         A2DIV
      *
     C     ACCOTP        IFEQ      'C'
     C     ACCOTP        OREQ      'A'
     C                   MOVE      'E'           A2TRTP
     C                   ENDIF
      *
     C     ACCOTP        IFEQ      'P'
     C     ACCOTP        OREQ      'V'
     C                   MOVE      'P'           A2TRTP
     C                   ENDIF
      *
     C                   MOVEL(P)  ACDESC        A2TYDS
     C                   Z-ADD     ACAMT         A2AMT
      *
     C     ACCOTP        IFEQ      'V'
     C                   MULT      -1            A2AMT
     C                   ENDIF
      *
     C                   Z-ADD     ACTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATX
     C                   MOVEL     SRDTO         X1DATE
     C                   EXSR      GETDAT
     C                   MOVEL     X2DATE        A2TNDT
      *
     C                   MOVEL(P)  ACCHCK        A2CHCK
     C                   Z-ADD     ACFND         A2FND
      *
     C                   Z-ADD     ACPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATX
     C                   MOVEL     SRDTO         X1DATE
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
     C                   MOVEL(P)  ACPODT        XXTMP
     C                   CAT(P)    XXTMP:0       A2KEY
      *
     C                   MOVEL(P)  ACTRDT        XXTMP
     C                   CAT(P)    XXTMP:0       A2KEY
      *
      *  ZZZ
      *
      ***** BEG CHG **** 10/09/03 ********************************
     C                   MOVEL     @@RNDT        ZZRNDT            8 0
     C                   MOVEL     ZZRNDT        X1DATE
     C************         MOVELDSDATE    X1DATE
      ***** END CHG **** 10/09/03 ********************************
     C                   EXSR      GETDAT
     C                   MOVEL     X2DATE        A2AODT
      *
      *  Write to output file
      *
     C                   WRITE     PAC02
     C***** BEGIN ADD ***** 09/25/01 **********************************
     C                   MOVE      *ON           *IN40
     C***** END ADD ******* 09/25/01 **********************************
      *
     C                   ENDSR                                                  SROUT
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
     C***** BEGIN ADD ***** 09/25/01 **********************************
      *================================================================
      * SRXAP - Get Agent Extract file
      *================================================================
     C     SRXAP         BEGSR
      *
      * If Have processed a fund, then update WHXAP/XAELFL flag to a 'Y'
     C     SVFND         IFGT      *ZEROS
     C     *IN40         ANDEQ     *ON
     C     XAKY          CHAIN     PXA01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     SVCO#         XACO#
     C                   Z-ADD     SVFND         XAFND
     C                   Z-ADD     XXPRDT        XAPRDT
     C                   Z-ADD     XXRNDT        XAEXDT
     C                   MOVE      'Y'           XAEXFL
     C                   MOVEL     Q1USER        XAUSER
     C                   MOVEL     XXPRDT        XADT
     C                   MOVE      DSTIME        XATIME
     C                   MOVEL     Q1PGM         XAPGM
     C                   MOVEL     Q1USER        XAIUSR
     C                   MOVEL     XXPRDT        XAIDT
     C                   MOVE      DSTIME        XAITIM
     C                   MOVEL     Q1PGM         XAIPGM
     C                   WRITE     PXA01
      *
     C                   ELSE
      *
     C                   MOVEL     Q1USER        XAUSER
     C                   MOVEL     XXPRDT        XADT
     C                   MOVE      DSTIME        XATIME
     C                   MOVEL     Q1PGM         XAPGM
     C                   Z-ADD     XXPRDT        XAPRDT
     C                   Z-ADD     XXRNDT        XAEXDT
     C                   MOVE      'Y'           XAEXFL
     C                   UPDATE    PXA01
      *
     C                   ENDIF                                                   End 60 On/0ff
      *
     C                   ENDIF                                                   End SVFND > 0
      *
      * Update The Main Extract Control File Too
     C     SVCO#         IFGT      *ZEROS
     C     *IN40         ANDEQ     *ON
     C     XCKY          CHAIN     PXC01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     XXRNDT        XCLD15
     C                   UPDATE    PXC01
      *
     C                   ENDIF                                                   End 60 On/0ff
     C                   ENDIF                                                   End SVFND > 0
      *
      *  Reset the CO# and GROUP Save Fields
     C                   MOVEL     ACCO#         SVCO#
     C                   MOVEL     ACFND         SVFND
     C                   MOVE      *OFF          *IN40
      *
     C                   ENDSR                                                  SRXAP
      *
     C*================================================================
     C***** END ADD ******* 09/25/01 **********************************
