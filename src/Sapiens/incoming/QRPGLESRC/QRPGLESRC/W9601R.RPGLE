     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W9601R - EXCESSIVE LOSS REPORT
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM DESCRIPTION:                                       *
     H*      THIS PROGRAM PRINTS A LISTING OF ALL EMPLOYERS WHO MEET  *
     H*      THE SELECTED CRITERIA FOR BEING CONSIDERED AN EXCESSIVE  *
     H*      LOSS MEMBER.                                             *
     H*                                                               *
     H*    PROJECT: 102AL                                             *
     H*    PROGRAMMER: 65                                             *
     H*                                                               *
     H*    CALLED FROM:                                               *
     H*      W9601C                                                   *
     H*                                                               *
     H*    CALLS TO:                                                  *
     H*      NONE                                                     *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/04/20 C:079 Project: 102WIH                                *
     H*                Add type of output parm. WFELP1 and WFELP2     *
     H*                                                               *
     H* 02/01/17 C:062 Support: 44995(102)                            *
     H*                Expand Loss Ration %                           *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 09/28/99 C:65 PROJECT: SUPPORT#19809                          *
     H*               MONTHLY PREMIUM IS RETURNING A DIFFERENT RESULT *
     H*               THAN THE YEAR TO DATE EARNED PREMIUM. USE THE   *
     H*               YEAR TO DATE EARNED PREMIUM INSTEAD SINCE ALL   *
     H*               OTHER REPORTS ARE USING THIS AS WELL.           *
     H*                                                               *
     H* 09/03/99 C:65 PROJECT: 102AL                                  *
     H*               CORRECTED THE DATE BEING USED FOR THE MONTHLY   *
     H*               EARNED PREMIUM CALCULATIONS.  THE APPROPRIATE   *
     H*               YEAR AND MONTH WERE NOT BEING MOVED INTO THE    *
     H*               DATE FIELD.  ALSO, IF THE LOSSES AS OF DATE IS  *
     H*               WITHIN THE LAST MONTH THEN USE THE DAYS FROM    *
     H*               THE LOSSES AS OF DATE INSTEAD OF THE LAST DAY   *
     H*               OF THE MONTH.                                   *
     H*                                                               *
     H*****************************************************************
     F*
     FWXEBL     IF   E           K DISK
     F*
     FWMFNL     IF   E           K DISK
     F*
     FWMEML     IF   E           K DISK
     F*
     FWDELL     IF   E           K DISK
     F*
     FWMCML5    IF   E           K DISK
     F*
     FWMREL     IF   E           K DISK
     F*
     FSMORL     IF   E           K DISK
     F*
     FSMCOL     IF   E           K DISK
     F*
     FWMUPL     IF   E           K DISK
     F*
     FWMRHL1    IF   E           K DISK
     F*** Beg Add *** 03/06/20 *********************************************
     FWFELP1    UF A E           K DISK
     FWFELP2    UF A E           K DISK
     F*** End Add *** 03/06/20 *********************************************
     F*
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOA)
     F*

     D MD              S              2  0 DIM(12) CTDATA PERRCD(12)
     D MON             S              9    DIM(12) CTDATA PERRCD(1)
     D FYR             S              3  0 DIM(3)                               YEAR
     D YMN             S              5  0 DIM(12)                              YEAR/MONTH
     D CLM             S              7  0 DIM(12)                              # CLAIMS
     D EPR             S             11  2 DIM(12)                              EARNED PREMIUM
     D TIN             S             11  2 DIM(12)                              TOTAL INCURRED
     D*** Begin Chg *** 02/01/17 *******************************
     D*LSR******       S              5  2 DIM(12)                              LOSS RATIO
     D LSR             S              8  3 DIM(12)                              LOSS RATIO
     D*** End   Chg *** 02/01/17 *******************************
     D*** Begin Add *** 02/01/17 *******************************
     D OVF             S              1    DIM(12)                              Over Flow
     D*** End   Add *** 02/01/17 *******************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXDATE                 1      7  0
     D  XXYYMM                 1      5  0
     D  XXYY                   1      3  0
     D  XXMM                   4      5  0
     D  XXDD                   6      7  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*****************************************************************
     C*   LIKE DEFINES
     C*****************************************************************
     C*
     C     *LIKE         DEFINE    EBRGN#        XXRGN#
     C     *LIKE         DEFINE    EBCO#         XXCO#
     C     *LIKE         DEFINE    EBFND         XXFND
     C     *LIKE         DEFINE    EBEMP#        XXEMP#
     C     *LIKE         DEFINE    EBDIV         XXDIV
     C     *LIKE         DEFINE    EBFYR         XXFYR
     C     *LIKE         DEFINE    EBFYR         XPFYR
     C*
     C*****************************************************************
     C*                   KEY LISTING
     C*****************************************************************
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   Key List for the Region Master File
     C*
     C     REKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXRGN#
     C*
     C*   KEY LIST FOR THE RESERVE HISTORY FILES
     C*
     C     RHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XPFYR
     C                   KFLD                    CMCASE
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XPFYR
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XPFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFDT             8
     C                   PARM                    QQTDT             8
     C                   PARM                    QQLSDT            8
     C*** Begin Chg *** 02/01/17 ***************************************
     C**********         PARM                    QQLSRA            5
     C                   PARM                    QQLSRA            6
     C*** End   Chg *** 02/01/17 ***************************************
     C                   PARM                    QQYEAR            1
     C                   PARM                    QQMNTH            2
     C                   PARM                    QQQUE            10
     C*** Beg Add *** 03/04/20 ******************************************
     C                   PARM                    QQTOO             1
     C*** End Add *** 03/04/20 ******************************************
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
       //*** Beg Add *** 03/04/20 ******************************************
      /Free
         If qqToO = 'F' or qqToO = 'B';
            Exec sql Delete from WFELP1 Where EL1user = :Q1user;
            Exec sql Delete from WFELP2 Where EL2user = :Q1user;
         Endif;
      /End-free
       //*** End Add *** 03/04/20 ******************************************
     C                   MOVE      QQFDT         X1FDT             8 0
     C                   Z-ADD     X1FDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDT             7 0
     C*
     C                   MOVE      QQTDT         X1TDT             8 0
     C                   Z-ADD     X1TDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT             7 0
     C*
     C                   MOVE      QQLSDT        X1LSDT            8 0
     C                   Z-ADD     X1LSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXLSDT            7 0
     C*** BEGIN ADD ***  09/03/99  ************************************
     C                   MOVEL     XXLSDT        XXLSYM            5 0
     C*** END ADD *****  09/03/99  ************************************
     C*
     C*** Begin Chg *** 02/01/17 ***************************************
     C**********         MOVE      QQLSRA        XXLSRA            5 2
     C                   MOVE      QQLSRA        XXLSRA            6 3
     C*** End   Chg *** 02/01/17 ***************************************
     C                   MOVE      QQYEAR        XXYEAR            1 0
     C                   MOVE      QQMNTH        XXMNTH            2 0
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C*
     C* FIRST CYCLE PROCESSING
     C*
     C                   EXSR      SR1ST
     C*
     C* START TO READ THE WORK FILE
     C*
     C                   READ      PEB01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C* SET KEY VALUES
     C*
     C                   Z-ADD     EBEMP#        XXEMP#
     C                   Z-ADD     EBDIV         XXDIV
     C                   Z-ADD     EBFYR         XXFYR
     C                   Z-ADD     EBRGN#        XXRGN#
     C*
     C*  GET EMPLOYER NAME AND INFORMATION
     C*
     C     EMKY          CHAIN     PEM01                              61
     C     *IN61         IFEQ      '0'
     C                   MOVEL     EMNAME        ENAME            40
     C                   MOVEL     EMLREP        ELREP             3
     C                   Z-ADD     EMEFD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EDATE             8 0
     C                   ELSE
     C                   MOVE      *BLANK        ENAME
     C                   MOVE      *BLANK        ELREP
     C                   Z-ADD     0             EDATE
     C                   ENDIF                                                  *IN61 IFEQ
     C*
     C*  Get region description
     C     REKY          CHAIN     PRE01                              62
     C     *IN62         IFEQ      *OFF
     C     RESTAT        ANDEQ     'A'
     C                   MOVEL(P)  REDESC        RDESC            30
     C                   ELSE
     C                   MOVE      *BLANKS       RDESC
     C                   END
     C*
     C* PROCESS PRIOR YEAR INFORMATION
     C*
     C     XXYEAR        IFNE      *ZERO
     C*
     C* CLEAR WORK ARRAYS
     C*
     C                   Z-ADD     0             FYR
     C                   Z-ADD     0             CLM
     C                   Z-ADD     0             EPR
     C                   Z-ADD     0             TIN
     C                   Z-ADD     0             LSR
     C*** Begin Add *** 02/01/17 *****************************************
     C                   MoveL     *Blanks       OVF
     C*** End   Add *** 02/01/17 *****************************************
     C*
     C     1             DO        3             X                 1 0
     C*
     C     XXFYR         SUB       X             XPFYR
     C     ELKY          CHAIN     PEL01                              63
     C     *IN63         IFEQ      *OFF
     C*
     C*   ADD PRIOR YEAR INFORMATION TO ARRAYS
     C*
     C                   Z-ADD     ELFYR         FYR(X)
     C                   MOVE      QQLSDT        SRDATE
     C                   EXSR      SRPRM
     C                   Z-ADD     XAMT1         EPR(X)
     C     CMKY          CHAIN     PCM01                              64
     C     *IN64         DOWEQ     *OFF
     C*
     C                   ADD       1             CLM(X)
     C*
     C                   ADD       CMAR01        TIN(X)
     C                   ADD       CMAR02        TIN(X)
     C                   ADD       CMAR03        TIN(X)
     C                   ADD       CMAR04        TIN(X)
     C                   ADD       CMAR05        TIN(X)
     C                   ADD       CMAR06        TIN(X)
     C                   ADD       CMAR07        TIN(X)
     C                   ADD       CMAR08        TIN(X)
     C                   ADD       CMAR09        TIN(X)
     C                   ADD       CMAR10        TIN(X)
     C                   ADD       CMRR01        TIN(X)
     C                   ADD       CMRR02        TIN(X)
     C                   ADD       CMRR03        TIN(X)
     C                   ADD       CMRR04        TIN(X)
     C                   ADD       CMRR05        TIN(X)
     C                   ADD       CMRR06        TIN(X)
     C                   ADD       CMRR07        TIN(X)
     C                   ADD       CMRR08        TIN(X)
     C                   ADD       CMRR09        TIN(X)
     C                   ADD       CMRR10        TIN(X)
     C*
     C     CMKY          READE     PCM01                                  64
     C                   ENDDO                                                  *IN64 DOWEQ
     C*
     C     EPR(X)        IFNE      *ZERO
     C*** Begin Chg *** 02/01/17 ********************************
     C*****TIN(X)        DIV(H)    EPR(X)        LSR(X)
     C**********         MULT      100           LSR(X)
     C                   If        (tin(x)/epr(x))*100 > 99999.999
     C                   Eval      ovf(x) = '+'
     C     tin(x)        div(h)    epr(x)        lsr(x)
     C                   mult      100           lsr(x)
     C                   Else
     C                   Eval      lsr(x) = %DecH(((tin(x)/epr(x))*100):8:3)
     C                   EndIf
     C*** End   Chg *** 02/01/17 ********************************
     C                   ELSE
     C                   Z-ADD     0             LSR(X)
     C                   ENDIF                                                  EPR,X IFNE
     C*
     C                   ELSE
     C                   LEAVE
     C                   ENDIF                                                  *IN63 IFEQ
     C*
     C                   ENDDO                                                  1 DO XXYEAR
     C*
     C*   COUNT # OF YEARS THAT MATCH OR EXCEED THE MINIMUM LOSS RATIO
     C*
     C                   Z-ADD     0             COUNT
     C     1             DO        3             X                 1 0
     C     LSR(X)        IFGE      XXLSRA
     C                   ADD       1             COUNT             2 0
     C                   ENDIF                                                  LSR,X IFGE
     C                   ENDDO                                                  1 DO 3
     C*
     C*   PRINT ACCOUNT IF IT HAS THE MINIMUM # OF EXCESSIVE LOSS YEARS
     C*
     C     COUNT         IFGE      XXYEAR
     C*
     C                   Z-ADD     0             XTCLM             7 0
     C                   Z-ADD     0             XTEPR            11 2
     C                   Z-ADD     0             XTTIN            11 2
     C*** Begin Chg *** 02/01/17 *******************************
     C**********         Z-ADD     0             XTLSR             5 2
     C                   Z-ADD     0             XTLSR             9 3
     C*** End   Chg *** 02/01/17 *******************************
     C*
     C     *INOF         IFEQ      *ON
     C*** Beg Add *** 03/04/20 *********************************************
     C     QQTOO         IFNE      'F'
     C*** End Add *** 03/04/20 *********************************************
     C                   EXCEPT    HEAD
     C*** Beg Add *** 03/04/20 *********************************************
     C                   ENDIF
     C*** End Add *** 03/04/20 *********************************************
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C*** Beg Add *** 03/04/20 *********************************************
     C     QQTOO         IFNE      'F'
     C*** End Add *** 03/04/20 *********************************************
     C                   EXCEPT    DETY1
     C*** Beg Add *** 03/04/20 *********************************************
     C                   ENDIF
     C*** End Add *** 03/04/20 *********************************************
     C*
     C     1             DO        3             X                 1 0
     C     FYR(X)        IFNE      *ZERO
     C*** Beg Add *** 03/04/20 *********************************************
     C     QQTOO         IFNE      'F'
     C*** End Add *** 03/04/20 *********************************************
     C                   EXCEPT    DETY
     C*** Beg Add *** 03/04/20 *********************************************
     C                   ENDIF
     C*** End Add *** 03/04/20 *********************************************
       //*** Beg Add *** 03/04/20 ******************************************
      /Free
                         If qqToO = 'F' or qqToO = 'B';
                            ExSr wrtWFELP1;
                         Endif;
      /End-free
       //*** End Add *** 03/04/20 ******************************************
     C                   ENDIF
     C                   ADD       CLM(X)        XTCLM
     C                   ADD       EPR(X)        XTEPR
     C                   ADD       TIN(X)        XTTIN
     C                   ADD       LSR(X)        XTLSR
     C                   ENDDO                                                  1 DO 3
     C                   DIV(H)    3             XTLSR
     C*
     C*** Beg Add *** 03/04/20 *********************************************
     C     QQTOO         IFNE      'F'
     C*** End Add *** 03/04/20 *********************************************
     C                   EXCEPT    DETYT
     C*** Beg Add *** 03/04/20 *********************************************
     C                   ENDIF
     C*** End Add *** 03/04/20 *********************************************
     C*
     C                   ADD       1             XRCNT             7 0
     C*
     C                   ENDIF                                                  COUNT IFGE
     C*
     C                   ENDIF                                                  XXYEAR IFNE
     C*
     C* PROCESS CONSECUTIVE MONTH INFORMATION
     C*
     C     XXMNTH        IFNE      *ZERO
     C*
     C* CLEAR WORK ARRAYS
     C*
     C                   Z-ADD     0             YMN
     C                   Z-ADD     0             CLM
     C                   Z-ADD     0             EPR
     C                   Z-ADD     0             TIN
     C                   Z-ADD     0             LSR
     C                   Z-ADD     0             X1PREM           11 2
     C*
     C                   Z-ADD     XXFYR         XPFYR
     C     ELKY          CHAIN     PEL01                              65
     C     *IN65         IFEQ      *OFF
     C                   MOVE      ELPFDT        XXDATE
     C                   Z-ADD     1             XXDD
     C*
     C     1             DO        12            Y                 2 0
     C                   Z-ADD     XXYYMM        YMN(Y)
     C                   ADD       1             XXMM
     C*
     C     XXMM          IFGT      12
     C                   Z-ADD     1             XXMM
     C                   ADD       1             XXYY
     C                   ENDIF
     C*
     C     XXDATE        IFGE      ELPTDT
     C     XXDATE        ORGE      XXLSDT
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDDO                                                  1 DO 12
     C*
     C     1             DO        12            Y                 2 0
     C*
     C     YMN(Y)        IFNE      *ZERO
     C*** BEGIN ADD ***  09/03/99  ************************************
     C                   Z-ADD     YMN(Y)        XXYYMM
     C     XXYYMM        IFEQ      XXLSYM
     C                   MOVE      XXLSDT        XXDD
     C                   ELSE
     C*** END ADD *****  09/03/99  ************************************
     C                   Z-ADD     MD(XXMM)      XXDD
     C*** BEGIN ADD ***  09/03/99  ************************************
     C                   ENDIF                                                  XXYYMM IFEQ
     C*** END ADD *****  09/03/99  ************************************
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT              8 0
     C                   MOVE      XXDT          SRDATE
     C                   EXSR      SRPRM
     C                   Z-ADD     XAMT1         XXPREM           11 2
     C*** BEGIN CHG ***  09/28/99  ************************************
     C********             ADD  XPREM1    X1PREM
     C********             Z-ADDX1PREM    EPR,Y
     C                   Z-ADD     XAMT1         EPR(Y)
     C*** END CHG *****  09/28/99  ************************************
     C*
     C     CMKY          CHAIN     PCM01                              66
     C     *IN66         DOWEQ     *OFF
     C*
     C     CMACDT        IFGE      ELPFDT
     C     CMACDT        ANDLE     XXDATE
     C*
     C     RHKY          CHAIN     PRH01                              68
     C     *IN68         DOWEQ     *OFF
     C     RHEFDT        IFLE      XXDATE
     C                   ADD       RHSU01        TIN(Y)
     C                   ADD       RHSU02        TIN(Y)
     C                   ADD       RHSU03        TIN(Y)
     C                   ADD       RHSU04        TIN(Y)
     C                   ADD       RHSU05        TIN(Y)
     C                   ADD       RHSU06        TIN(Y)
     C                   ADD       RHSU07        TIN(Y)
     C                   ADD       RHSU08        TIN(Y)
     C                   ADD       RHSU09        TIN(Y)
     C                   ADD       RHSU10        TIN(Y)
     C                   ENDIF                                                  PHEFDT IFLE
     C     RHKY          READE     PRH01                                  68
     C                   ENDDO                                                  *IN68 DOWEQ
     C*
     C                   ENDIF                                                  CMACDT IFGE
     C*
     C     CMKY          READE     PCM01                                  66
     C                   ENDDO                                                  *IN66 DOWEQ
     C*
     C     EPR(Y)        IFNE      *ZERO
     C*** Begin Chg *** 02/01/17 ********************************
     C*****TIN(Y)        DIV(H)    EPR(Y)        LSR(Y)
     C**********         MULT      100           LSR(Y)
     C                   If        (tin(y)/epr(y))*100 > 99999.999
     C                   Eval      ovf(y) = '+'
     C     tin(y)        div(h)    epr(y)        lsr(y)
     C                   mult      100           lsr(y)
     C                   Else
     C                   Eval      lsr(y) = %DecH(((tin(y)/epr(y))*100):8:3)
     C                   EndIf
     C*** End   Chg *** 02/01/17 ********************************
     C                   ELSE
     C                   Z-ADD     0             LSR(Y)
     C                   ENDIF                                                  EPR,Y IFNE
     C*
     C                   ENDIF                                                  YMN,Y IFNE
     C*
     C                   ENDDO                                                  1 DO 12
     C*
     C*   COUNT # OF CONSECUTIVE MONTHS THAT MATCH OR EXCEED THE RATIO
     C*
     C                   Z-ADD     0             COUNT
     C     1             DO        12            Y
     C     LSR(Y)        IFGE      XXLSRA
     C                   ADD       1             COUNT             2 0
     C                   ENDIF                                                  LSR,Y IFGE
     C                   ENDDO                                                  1 DO 12
     C*
     C*   PRINT ACCOUNT IF IT HAS THE MINIMUM # OF CONSECUTIVE MONTHS
     C*
     C     COUNT         IFGE      XXMNTH
     C*
     C                   Z-ADD     0             XTCLM             7 0
     C                   Z-ADD     0             XTEPR            11 2
     C                   Z-ADD     0             XTTIN            11 2
     C*** Begin Chg *** 02/01/17 *******************************
     C**********         Z-ADD     0             XTLSR             5 2
     C                   Z-ADD     0             XTLSR
     C*** End   Chg *** 02/01/17 *******************************
     C*
     C     *INOA         IFEQ      *ON
     C*** Beg Add *** 03/04/20 *********************************************
     C     QQTOO         IFNE      'F'
     C*** End Add *** 03/04/20 *********************************************
     C                   EXCEPT    HEAD1
     C*** Beg Add *** 03/04/20 *********************************************
     C                   ENDIF
     C*** End Add *** 03/04/20 *********************************************
     C                   MOVE      *OFF          *INOA
     C                   ENDIF
     C*
     C*** Beg Add *** 03/04/20 *********************************************
     C     QQTOO         IFNE      'F'
     C*** End Add *** 03/04/20 *********************************************
     C                   EXCEPT    DET1
     C*** Beg Add *** 03/04/20 *********************************************
     C                   ENDIF
     C*** End Add *** 03/04/20 *********************************************
     C*
     C     1             DO        12            Y
     C*
     C     YMN(Y)        IFNE      *ZERO
     C                   MOVEL     YMN(Y)        XYR               3 0
     C     XYR           ADD       1900          YEAR              4 0
     C                   MOVE      YMN(Y)        XMN               2 0
     C                   MOVEL(P)  MON(XMN)      MONTH             9
     C*** Beg Add *** 03/04/20 *********************************************
     C     QQTOO         IFNE      'F'
     C*** Eng Add *** 03/04/20 *********************************************
     C                   EXCEPT    DETM
     C*** Beg Add *** 03/04/20 *********************************************
     C                   ENDIF
     C*** End Add *** 03/04/20 *********************************************
       //*** Beg Add *** 03/04/20 ******************************************
      /Free
                         If qqToO = 'F' or qqToO = 'B';
                            ExSr wrtWFELP2;
                         Endif;
      /End-free
       //*** End Add *** 03/04/20 ******************************************
     C                   ELSE
     C                   LEAVE
     C                   ENDIF                                                  YMN,Y IFNE
     C*
     C                   ENDDO                                                  1 DO 12
     C*
     C*** Beg Add *** 03/04/20 *********************************************
     C     QQTOO         IFNE      'F'
     C*** End Add *** 03/04/20 *********************************************
     C                   EXCEPT    DETM1
     C*** Beg Add *** 03/04/20 *********************************************
     C                   ENDIF
     C*** End Add *** 03/04/20 *********************************************
     C*
     C                   ADD       1             XRCNT1            7 0
     C*
     C                   ENDIF                                                  COUNT IFGE
     C*
     C                   ENDIF                                                  *IN65 IFEQ
     C*
     C                   ENDIF                                                  XXMNTH IFNE
     C*
     C*   READ THE NEXT RECORD
     C*
     C                   READ      PEB01                                  60
     C*
     C*   WRITE REGION TOTALS ON A REGION CHANGE
     C*
     C     *IN60         IFEQ      *OFF
     C     EBRGN#        ANDNE     XXRGN#
     C*
     C     XRCNT         IFNE      *ZERO
     C*** Beg Add *** 03/04/20 *********************************************
     C     QQTOO         IFNE      'F'
     C*** End Add *** 03/04/20 *********************************************
     C                   EXCEPT    DETR
     C*** Beg Add *** 03/04/20 *********************************************
     C                   ENDIF
     C*** End Add *** 03/04/20 *********************************************
     C                   MOVE      *ON           *INOF
     C                   ADD       XRCNT         XTOT
     C                   Z-ADD     0             XRCNT
     C                   ENDIF                                                  XRCNT IFNE
     C*
     C     XRCNT1        IFNE      *ZERO
     C*** Beg Add *** 03/04/20 *********************************************
     C     QQTOO         IFNE      'F'
     C*** End Add *** 03/04/20 *********************************************
     C                   EXCEPT    DETR1
     C*** Beg Add *** 03/04/20 *********************************************
     C                   ENDIF
     C*** End Add *** 03/04/20 *********************************************
     C                   MOVE      *ON           *INOA
     C                   ADD       XRCNT1        XTOT1
     C                   Z-ADD     0             XRCNT1
     C                   ENDIF                                                  XRCNT1 IFNE
     C*
     C                   ENDIF                                                  *IN60 DOWEQ
     C*
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* PRINT FINAL TOTALS
     C*
     C     XRCNT         IFNE      *ZERO
     C*** Beg Add *** 03/04/20 *********************************************
     C     QQTOO         IFNE      'F'
     C*** End Add *** 03/04/20 *********************************************
     C                   EXCEPT    DETR
     C*** Beg Add *** 03/04/20 *********************************************
     C                   ENDIF
     C*** End Add *** 03/04/20 *********************************************
     C                   ADD       XRCNT         XTOT
     C                   ENDIF                                                  XRCNT IFNE
     C*
     C     XTOT          IFNE      *ZERO
     C*** Beg Add *** 03/04/20 *********************************************
     C     QQTOO         IFNE      'F'
     C*** End Add *** 03/04/20 *********************************************
     C                   EXCEPT    DETT
     C*** Beg Add *** 03/04/20 *********************************************
     C                   ENDIF
     C*** End Add *** 03/04/20 *********************************************
     C                   ENDIF                                                  XRCNT IFNE
     C*
     C     XRCNT1        IFNE      *ZERO
     C*** Beg Add *** 03/04/20 *********************************************
     C     QQTOO         IFNE      'F'
     C*** End Add *** 03/04/20 *********************************************
     C                   EXCEPT    DETR1
     C*** Beg Add *** 03/04/20 *********************************************
     C                   ENDIF
     C*** End Add *** 03/04/20 *********************************************
     C                   ADD       XRCNT1        XTOT1
     C                   ENDIF                                                  XRCNT1 IFNE
     C*
     C     XTOT1         IFNE      *ZERO
     C*** Beg Add *** 03/04/20 *********************************************
     C     QQTOO         IFNE      'F'
     C*** End Add *** 03/04/20 *********************************************
     C                   EXCEPT    DETT1
     C*** Beg Add *** 03/04/20 *********************************************
     C                   ENDIF
     C*** End Add *** 03/04/20 *********************************************
     C                   ENDIF                                                  XRCNT IFNE
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       //*** Beg Add *** 03/04/20 ******************************************
         If qqToO <> 'F';
       //*** End Add *** 03/04/20 ******************************************
          Except Route;
          Except Route1;
       //*** Beg Add *** 03/04/20 ******************************************
         Endif;
       //*** End Add *** 03/04/20 ******************************************
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C     UPKY          CHAIN     WMUPL                              60
     C*
     C                   TIME                    DSDTTM
     C*
     C     COKY          CHAIN     SMCOL                              60
     C*
     C*  CENTER TEXT IN FIELD
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        CONAME
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*  GET FUND DESCRIPTION
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           60
     C                   ELSE
     C                   MOVE      *BLANKS       XFNAME
     C                   END
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C*
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF                                                  N<=60
     C*
     C                   ENDIF                                                  ORTITL<>*BLANKS
     C*
     C*  CENTER TEXT IN FIELD
     C*
     C                   MOVEL     ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        ORTITL
     C*
     C                   END
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *************************************************
     C*********          EXCEPT    ROUTE
     C*********          EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 *************************************************
     C*
     C                   MOVE      *ON           *INOF
     C                   MOVE      *ON           *INOA
     C                   Z-ADD     0             XTOT              7 0
     C                   Z-ADD     0             XTOT1             7 0
     C*
     C                   ENDSR
     C*
      *==================================================================*
      *   Compute Earned Premium to Date   ** Subroutine **   S R P R M  *
      *==================================================================*
     C*
     CSR   SRPRM         BEGSR
     C**
     C**    CALL PGM TO CALCULATE EARNED PREMIUM YEAR-TO-DATE
     C**                      AND EARNED PREMIUM CURRENT PERIOD
     C**
     C                   Z-ADD     *ZERO         XAMT1
     C                   Z-ADD     *ZERO         XPREM1
     C                   MOVEL     EBCO#         SRCO#             3
     C                   MOVEL     EBFND         SRFND             3
     C                   MOVEL     XPFYR         SRFYR             3
     C                   MOVEL     EBEMP#        SREMP#            9
     C                   MOVEL     EBDIV         SRDIV             5
     C                   MOVE      ' '           QQERR             1
     C                   MOVE      *BLANK        SRAMT            11
     C                   MOVE      *BLANK        SRAMT2           11
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRPRT1'
     C                   PARM                    SRCO#
     C                   PARM                    SRFND
     C                   PARM                    SRFYR
     C                   PARM                    SREMP#
     C                   PARM                    SRDIV
     C                   PARM                    SRDATE            8
     C                   PARM                    SRAMT
     C                   PARM                    SRAMT2
     C                   PARM                    QQTERM
     C                   MOVE      SRAMT         XAMT1            11 2
     C                   MOVE      SRAMT2        XPREM1           11 2
     C*
     C                   ENDSR
     C*
       //*** Beg Add *** 03/04/20 ******************************************
      /Free
       //===================================================================
       //  Subroutine to write WFELP1
       //===================================================================
        Begsr wrtWfelp1;
           EL1co# = xxCo#;
           EL1fnd = xxFnd;
           EL1emp# = EBemp#;
           EL1div = EBdiv;
           EL1fYr = EBfYr;
           EL1cNam = %Trim(COname);
           EL1fNam = %Trim(FNname);
           EL1eNam = %Trim(EMname) + %trim(EMnam2);
           EL1rgn# = EMrgn#;
           EL1rNam = rDesc;
           EL1lRep = EMlRep;
           EL1efDt = Edate;
           EL1year = Fyr(x);
           EL1#clm = Clm(x);
           EL1ePrm = Epr(x);
           EL1tInc = Tin(x);
           EL1oVrF = Ovf(x);
           EL1lsR  = Lsr(x);
           EL1user = Q1user;
           EL1pgm  = Q1pgm;
      /End-Free
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EL1Dt
     C                   Z-ADD     DSTIME        EL1time
      /Free
           Write pel101;
        Endsr;
       //===================================================================
       //  Subroutine to write WFELP2
       //===================================================================
        Begsr wrtWfelp2;
           EL2co# = xxCo#;
           EL2fnd = xxFnd;
           EL2emp# = EBemp#;
           EL2div = EBdiv;
           EL2fYr = EBfYr;
           EL2cNam = %Trim(COname);
           EL2fNam = %Trim(FNname);
           EL2eNam = %Trim(EMname) + %trim(EMnam2);
           EL2rgn# = EMrgn#;
           EL2rNam = rDesc;
           EL2prem = xxPrem;
           EL2month = Month;
           EL2year = Year;
           EL2oVrF = Ovf(y);
           EL2lsR  = Lsr(y);
           EL2user = Q1user;
           EL2pgm  = Q1pgm;
      /End-Free
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EL2Dt
     C                   Z-ADD     DSTIME        EL2time
      /Free
           Write pel201;
        Endsr;
      /End-Free
       //*** End Add *** 03/04/20 ******************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              60
      ***** End Del ***** 08/26/14 *************************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *************************************************
     O          E            ROUTE       2  2
     O                                           18 'Group:    '
     O                       XXFND         Z     36
     O          E            ROUTE          2
     O                                           20 'From Date:  '
     O                       X1FDT               36 '  /  /    '
     O          E            ROUTE          2
     O                                           20 'To Date:    '
     O                       X1TDT               36 '  /  /    '
     O          E            ROUTE          2
     O                                           21 'Losses As Of:'
     O                       X1LSDT              36 '  /  /    '
     O          E            ROUTE          2
     O                                           21 'Loss Ratio:  '
     O                       XXLSRA        3     36
     O          E            ROUTE          2
     O                                           21 '# of Years:  '
     O                       XXYEAR        Z     36
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                24 '(C) PROGRESSIVE DATA '
     O***                                40 'SOLUTIONS, INC.'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              86
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                           23 'Policies exceeding loss'
     O                                           25 'es'
     O                       XXYEAR              27
     O                                           48 'out of last 3 years.'
     O          E            HEAD           2
     O                                           13 'Losses as of:'
     O                       X1LSDT              24 '  /  /    '
     O                                           51 'Loss ratio in excess of:'
     O*** Begin Chg *** 02/01/17 ***************************************
     O**********             XXLSRA        3     59
     O**********                                 60 '%'
     O                       XXLSRA        3     60
     O                                           61 '%'
     O*** End   Chg *** 02/01/17 ***************************************
     O                                           85 'Policy effective date'
     O                                           92 'range:'
     O                       X1FDT              103 '  /  /    '
     O                                          105 '-'
     O                       X1TDT              116 '  /  /    '
     O          E            HEAD           2
     O                                            7 'Group :'
     O                       XXFND         Z     11
     O                       XFNAME              72
     O          E            HEAD           3
     O                                            7 'Region:'
     O                       XXRGN#        Z     11
     O                       RDESC               42
     O          E            HEAD           1
     O                                           60 'Fld'
     O                                           71 'Inception'
     O                                           86 '# of'
     O                                          102 'Earned'
     O                                          115 'Total'
     O*** Begin Chg *** 02/01/17 *********************************
     O**********                                127 'Loss'
     O                                          130 'Loss'
     O*** End   Chg *** 02/01/17 *********************************
     O          E            HEAD           2
     O                                            9 'Employer#'
     O                                           15 'Div'
     O                                           20 'Name'
     O                                           60 'Rep'
     O                                           68 'Date'
     O                                           77 'Year'
     O                                           88 'Claims'
     O                                          103 'Premium'
     O                                          118 'Incurred'
     O*** Begin Chg *** 02/01/17 *********************************
     O**********                                128 'Ratio'
     O                                          131 'Ratio'
     O*** End   Chg *** 02/01/17 *********************************
     O          E            DETY1          1
     O                       XXEMP#        Z      9
     O                       XXDIV         Z     15
     O                       ENAME               56
     O                       ELREP               60
     O                       EDATE               71 '  /  /    '
     O          E            DETY           1
     O                       FYR(X)        Z     77
     O                       CLM(X)        2     88
     O                       EPR(X)        J    104
     O                       TIN(X)        J    119
     O*** Begin Chg *** 02/01/17 **************************************
     O**********             LSR(X)        3    128
     O**********                                129 '%'
     O                       OVF(X)             122
     O                       LSR(X)        3    131
     O                                          132 '%'
     O*** End   Chg *** 02/01/17 **************************************
     O          E            DETYT       1  3
     O                                           73 'Totals:'
     O                       XTCLM         2     88
     O                       XTEPR         J    104
     O                       XTTIN         J    119
     O*** Begin Chg *** 02/01/17 **************************************
     O**********             XTLSR         3    128
     O**********                                129 '%'
     O                       XTLSR         3    131
     O                                          132 '%'
     O*** End   Chg *** 02/01/17 **************************************
     O          E            DETR           2
     O                                           24 'Total number of policies'
     O                                           34 'exceeding'
     O                       XXYEAR              36
     O                                           55 'out of 3 years for'
     O                                           62 'region'
     O                       XXRGN#              66
     O                                           67 ':'
     O                       XRCNT         3     78
     O          E            DETT           2
     O                                           24 'Total number of policies'
     O                                           34 'exceeding'
     O                       XXYEAR              36
     O                                           59 'out of 3 years for all'
     O                                           68 'regions:'
     O                       XTOT          3     78
     OQSYSPRT1  E            ROUTE1         1 06
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              60
      ***** End Del ***** 08/26/14 *************************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********E            ROUTE1         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE1         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *************************************************
     O          E            ROUTE1      2  2
     O                                           18 'Group:    '
     O                       XXFND         Z     36
     O          E            ROUTE1         2
     O                                           20 'From Date:  '
     O                       X1FDT               36 '  /  /    '
     O          E            ROUTE1         2
     O                                           20 'To Date:    '
     O                       X1TDT               36 '  /  /    '
     O          E            ROUTE1         2
     O                                           21 'Losses As Of:'
     O                       X1LSDT              36 '  /  /    '
     O          E            ROUTE1         2
     O                                           21 'Loss Ratio:  '
     O                       XXLSRA        3     36
     O          E            ROUTE1         2
     O                                           21 '# of Months: '
     O                       XXMNTH        Z     36
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE1
     O***                                24 '(C) PROGRESSIVE DATA '
     O***                                40 'SOLUTIONS, INC.'
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              86
     O                       DSDATE        Y    132
     O          E            HEAD1          3
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE1         Z    132
     O          E            HEAD1          1
     O                                           23 'Policies exceeding loss'
     O                                           25 'es'
     O                       XXMNTH        Z     28
     O                                           51 'consecutive months in '
     O                                           71 'current policy year.'
     O          E            HEAD1          2
     O                                           13 'Losses as of:'
     O                       X1LSDT              24 '  /  /    '
     O                                           51 'Loss ratio in excess of:'
     O*** Begin Chg *** 02/01/17 ***************************************
     O**********             XXLSRA        3     59
     O**********                                 60 '%'
     O                       XXLSRA        3     60
     O                                           61 '%'
     O*** End   Chg *** 02/01/17 ***************************************
     O                                           85 'Policy effective date'
     O                                           92 'range:'
     O                       X1FDT              103 '  /  /    '
     O                                          105 '-'
     O                       X1TDT              116 '  /  /    '
     O          E            HEAD1          2
     O                                            7 'Group :'
     O                       XXFND         Z     11
     O                       XFNAME              72
     O          E            HEAD1          3
     O                                            7 'Region:'
     O                       XXRGN#        Z     11
     O                       RDESC               42
     O          E            HEAD1          1
     O                                           60 'Fld'
     O                                           71 'Inception'
     O                                           90 'Earned'
     O*** Begin Chg *** 02/01/17 *********************************
     O**********                                115 'Loss'
     O                                          118 'Loss'
     O*** End   Chg *** 02/01/17 *********************************
     O          E            HEAD1          2
     O                                            9 'Employer#'
     O                                           15 'Div'
     O                                           20 'Name'
     O                                           60 'Rep'
     O                                           68 'Date'
     O                                           77 'Year'
     O                                           91 'Premium'
     O                                           99 'Month'
     O                                          108 'Year'
     O*** Begin Chg *** 02/01/17 *********************************
     O**********                                116 'Ratio'
     O                                          119 'Ratio'
     O*** End   Chg *** 02/01/17 *********************************
     O          E            DET1           1
     O                       XXEMP#        Z      9
     O                       XXDIV         Z     15
     O                       ENAME               56
     O                       ELREP               60
     O                       EDATE               71 '  /  /    '
     O                       XXFYR         Z     77
     O                       XXPREM        J     92
     O          E            DETM           1
     O                       MONTH              103
     O                       YEAR               108
     O*** Begin Chg *** 02/01/17 **************************************
     O**********             LSR(Y)        3    116
     O**********                                117 '%'
     O                       OVF(Y)             110
     O                       LSR(Y)        3    119
     O                                          120 '%'
     O*** End   Chg *** 02/01/17 **************************************
     O          E            DETM1          1
     O          E            DETR1          2
     O                                           24 'Total number of policies'
     O                                           34 'exceeding'
     O                       XXMNTH        3     37
     O                                           60 'consecutive months for'
     O                                           67 'region'
     O                       XXRGN#              71
     O                                           72 ':'
     O                       XRCNT1        3     82
     O          E            DETT1          2
     O                                           24 'Total number of policies'
     O                                           34 'exceeding'
     O                       XXMNTH        3     37
     O                                           60 'consecutive months for'
     O                                           73 'all regions:'
     O                       XTOT1         3     82
**
312831303130313130313031
**
January
February
March
April
May
June
July
August
September
October
November
December
