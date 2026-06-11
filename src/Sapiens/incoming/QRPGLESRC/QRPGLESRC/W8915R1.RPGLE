     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8915R1 - WORK FILE BUILD - AGT COMM - WHACP
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
     H* 03/25/04 C: 132 CI010                                         *
     H*           - Add start Date PARM for Job Date, so each file    *
     H*             has same ASOF DATE, for next incremetal.          *
     H*                                                               *
     H* 03/10/04 C: 58  CI015
     H*             Add start fund year PARM; extract only therefrom.
     H*                                                               *
     H* 10/02/01  C:132 PROJECT WC924                                 *
     H*               - Change: Add a check new FNCICF flag in WMFNP  *
     H*                 for a 'Y' to include in Extract = Else Bypass *
     H*                                                               *
     H* 09/25/01 C:132 PROJECT WC924                                  *
     H*    Changed to Use input WMACL5 and check the WHXAP file for
     H*    Valid Extract Records by CO#, FND, Check for Prior
     H*    Historical Extract first, Do not Process if no Historical  *
     H*    Also ADDED 2nd Input Parm for CO#                          *
     H*                                                               *
     H* 08/01/01 C:132 PROJECT WC924                                  *
     H*    Changed to Use input DATE PARM as the Last Extract Date
     H*    Will only include records from WMACP that have a Maint
     H*    Date (ACDT) Greater than the Last Extract Date             *
     H*    COPIED FROM W8915R2                                        *
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
      *
      * AGT COMM WORK FILE
     F*****WMACP   IF  E           K        DISK
     FWMACL5    IF   E           K DISK
      * AGT ASSIGNMENT FILE
     FWMAAL1    IF   E           K DISK
      *
      * FUND
     FWMFNL     IF   E           K DISK
     F**** Begin Add ****  07/22/05  **************************************
     F*   Employer Carrier File
     FWMRPL     IF   E           K DISK
     F**** End Add ******  07/22/05  **************************************
     F*
     F*  Agnet Extract Control File
     FWHXAP     UF   E           K DISK
     F*
     F* Output - Data Warehouse - WHARP
     FWHACP     IF A E             DISK
     F                                     RENAME(PAC01:PAC02)
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
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
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

     C*
     C************************************************************
     C*  Begin
     C************************************************************
      *
      *  READ WORK FILE
      *
     C     ACKY          SETLL     PAC01
     C                   READE     PAC01                                  60
     C     *IN60         DOWEQ     '0'
      *
      *   If CO# not same as Parm Passed end program
      *
     C     ACCO#         IFNE      ##CO#
     C                   GOTO      TERM
     C                   ENDIF
      *
      *   Selection: bypass all test groups.
      *
     C     ACCO#         IFEQ      *ZERO
     C                   GOTO      NEXT
     C                   ENDIF
      *
     C     ACFND         IFNE      SVFND
     C                   EXSR      SRXAP
     C     FNKY          CHAIN     PFN01                              60
     C                   ENDIF
      *
     C     FNTEST        IFEQ      'Y'
     C     FNCICF        ORNE      'Y'
     C     *IN60         OREQ      *ON
     C     *IN40         OREQ      *OFF
     C                   GOTO      NEXT
     C                   ENDIF
      *
      *   Selection: WMACP Maint Date LEGT Last Extract Date.
      *
     C     ACDT          IFLE      ##PRDT
     C***** BEG ADD ******* 03/10/04 **********************************
     C     ACFYR         ORLT      @@STYR
     C***** END ADD ******* 03/10/04 **********************************
     C                   GOTO      NEXT
     C                   ENDIF
      *
      * WRITE TO OUTPUT
      *
     C                   EXSR      SROUT
      *
     C     NEXT          TAG
     C                   READ      PAC01                                  60
     C                   ENDDO
      *
      * End of Processing, Terminate Program
      *
     C     TERM          TAG
      *
     C                   MOVE      '1'           *INLR
      *
      * Update the Commission Extract Control File at Program End
     C                   EXSR      SRXAP
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
     C     ACCOTP        IFEQ      'C'                                          E --> C 8/16/01
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
      ***** BEG CHG ***** 03/25/04 ************************************
      * Change the Job Parm Date into AS OF Date instead of Extract Date
     C                   MOVEL     @@JBDT        ZZRNDT            8 0
     C                   MOVEL     ZZRNDT        X1DATE
     C************         MOVELDSDATE    X1DATE
      ***** END CHG ***** 03/25/04 ************************************
     C                   EXSR      GETDAT
     C                   MOVEL     X2DATE        A2AODT
      *
      *  Write to output file
      *
     C                   WRITE     PAC02
     C                   ENDSR                                                  SROUT
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
      *================================================================
      * SRXAP - Get Agent Extract file
      *================================================================
     C     SRXAP         BEGSR
      *
      * If Have processed a fund, then update WHXAP/XAELFL flag to a 'Y'
     C     SVFND         IFGT      *ZEROS
     C     *IN40         ANDEQ     *ON
     C     XAKY3         CHAIN     PXA01                              60
     C                   MOVE      'Y'           XAEXFL
     C                   Z-ADD     XCRNDT        XAEXDT            7 0
     C                   MOVEL     Q1USER        XAUSER
     C                   MOVEL     XCRNDT        XADT
     C                   MOVE      DSTIME        XATIME
     C                   MOVEL     Q1PGM         XAPGM
     C                   UPDATE    PXA01                                60
     C                   ENDIF
      *
      * If End of Program, return after Update WHXAP
     C     *INLR         CABEQ     *ON           ENDXA
      *
     C                   MOVE      *HIVAL        XXPRDT
     C                   MOVE      *OFF          *IN40
     C     XAKY          SETGT     PXA01
      *
     C     XAKY2         READE     PXA01                                  60
      *
      * File is in Descending Date order, so Each READ will retrieve
      * the Next Oldest Date within the CO# and Group
      * If Hit on Agent Extract file, Save Group FND & Agt Extract Date
      * If Process flag (XAEXFO) is not a Y, Read 2nd Time-Older Date
     C     *IN60         DOWEQ     *OFF
     C                   MOVEL     XAFND         SVFND             3 0
     C     XAEXFL        IFEQ      'Y'
     C                   Z-ADD     XAPRDT        ##PRDT            7 0
     C                   MOVE      *ON           *IN40
     C                   LEAVE
     C                   ELSE
     C                   Z-ADD     XAPRDT        SVPRDT
     C     XAKY2         READE     PXA01                                  60
     C                   ENDIF                                                  XAEXFL = Y
     C                   ENDDO                                                  60 = *OFF
      *
     C                   MOVEL     ACFND         SVFND
     C     ENDXA         TAG
      *
     C                   ENDSR                                                  SRXAP
      *
     C*================================================================
     C*      *INZSR  Initialization
     C*================================================================
     C     *INZSR        BEGSR
     C*
     C     *LIKE         DEFINE    ACPODT        XXPRDT
     C     *LIKE         DEFINE    ACPODT        SVPRDT
      ********************
      *  PLIST definitions
      ********************
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@RNDT            8
      ***** BEG ADD ***** 03/25/04 ************************************
     C                   PARM                    @@JBDT            8
      ***** END ADD ***** 03/25/04 ************************************
     C***** BEG ADD ******* 03/10/04 **********************************
     C                   PARM                    @@STYR            3 0
     C***** END ADD ******* 03/10/04 **********************************
     C                   MOVE      @@CO#         ##CO#             3 0
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
      *
      *********
      *  KLISTS
      *********
      *
      *  Agent Extract Control
     C     XAKY          KLIST
     C                   KFLD                    ACCO#
     C                   KFLD                    ACFND
     C                   KFLD                    XXPRDT
      *
     C     XAKY2         KLIST
     C                   KFLD                    ACCO#
     C                   KFLD                    ACFND
      *
     C     XAKY3         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVPRDT
      *
      *  CICenter Extract Control
     C     XCKY          KLIST
     C                   KFLD                    @@CO#
      *
      *  Agent Comm Work File
     C     ACKY          KLIST
     C                   KFLD                    ##CO#
      *
      *  Fund
     C     FNKY          KLIST
     C                   KFLD                    ACCO#
     C                   KFLD                    ACFND
      *
     C     AAKY          KLIST
     C                   KFLD                    ACCO#
     C                   KFLD                    ACFND
     C                   KFLD                    ACAGT#
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
      *************
      *  INITIALIZE
      *************
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
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XCRNDT            7 0
      *
     C                   ENDSR
      * ============================================================= *
