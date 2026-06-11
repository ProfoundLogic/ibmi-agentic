     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7329R - SUMMARY INSTALLMENT SCHEDULE PRINT
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:                                                 *
     H*    This program produces a summary installment schedule for   *
     H*    the policy passed in to program.  All H and F transactions *
     H*    will apply to the first installment regardless of their    *
     H*    posting date.  All payments will be accumulated and        *
     H*    disolved over the installment amounts, starting with the   *
     H*    first, until it is gone.  The report generated will lump   *
     H*    all divisions together to get totals.                      *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W7329C                                                     *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 10/22/98  PROGRAMMER- C:87  PROJECT: 96WAO          *
     H*                                                               *
     H*****************************************************************
     H*   INDICATORS:                                                 *
     H*                                                               *
     H*   08 - Batch mode                                             *
     F*   09 - Demand mode                                            *
     H*   60 - General chain                                          *
     H*   71 - Comment 1 not in file                                  *
     H*   72 - Comment 2 not in file                                  *
     H*   73 - Comment 3 not in file                                  *
     H*   75 - Audited policy                                         *
     H*   76 - EMFDNM is not blank                                    *
     H*   77 - Print msg for no installments due                      *
     H*   78 - Posting date is blank                                  *
     H*                                                               *
     H*****************************************************************
     H**  REVISIONS:                                                  *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 10/06/10 C:136 Project: 71WKU                                 *
     H*                Exclude Retro Payments from premium payments.  *
     H*                Subtract from installment amounts.             *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 12/11/08 C:33  Project; WC1769                                *
     H*                Add 6th and 7th assessments.                   *
     H*                                                               *
     H* 10/07/05 C:65  PROJECT: WC1317                                *
     H*                PRINT EXPANDED NAME FIELDS FOR EMPLOYER NAMES. *
     H*                                                               *
     H*  07/02/04 C:136 Project: WC1227 (REF 109DL)                   *
     H*                 Include 4th and 5th assessments when accumu-  *
     H*                 lating fees accross all divisions.            *
     H*                                                               *
     H* 02/19/04 C:136 PROJECT: WC1033                                *
     H*                Add code for new A/R type of 'T' (True Void)   *
     H*                                                               *
     H*  05/08/03 C:132 Project: WC1117                               *
     H*                 REMOVED LOCK BOX address check in group       *
     H*                 auxiliary file and remove use of this address *
     H*                                                               *
     H*  02/13/03 C:132 Project: WC1117                               *
     H*                 Added LOCK BOX address check in group         *
     H*                 auxiliary file. If present in WMF2P, use the  *
     H*                 Lock box address instead of accounting office *
     H*                 address.                                      *
     H*                                                               *
     H* 01/02/01 C:65  PROJECT: 109CO                                 *
     H*                 ADD 3RD ASSESSMENTS FOR A FEE.                *
     H*                                                               *
     H* 01/24/00 C:33  PROJECT: WC577                                 *
     H*                ADD VOIDS, DON'T SUBTRACT THEM.                *
     H*                                                               *
     H* 12/10/98 C:61 SUPPORT 14595                                   *
     H*               Only process active (WMA3P/A3STAT = 'A')        *
     H*               Employer 3rd Address records.                   *
     H**                                                              *
     H**  11/09/98 C:65 PROJECT: 96WAO                                *
     H**                ADD LOGO TO THE SCHEDULE.                     *
     H**                                                              *
     H*****************************************************************
     F*
     FWMEAL15   IF   E           K DISK
     FWMEAL8    IF   E           K DISK
     F                                     RENAME(PEA01:PEA08)
     FWMEML     IF   E           K DISK
     FWDELL1    IF   E           K DISK
     FWMOFL     IF   E           K DISK
     FWMA3L     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     F*** BEGIN ADD ****** 08/09/10 ************************************
     FWDF1L     IF   E           K DISK
     F*** END   ADD ****** 08/09/10 ************************************
     F*** BEGIN CHG ***  11/09/98  ************************************
     F*****QSYSPRT O   F      85     OA     PRINTER
     FWDF2L     IF   E           K DISK
     FQSYSPRT   O    F   85        PRINTER OFLIND(*INOA)
     F                                     USROPN
     F*** END CHG *****  11/09/98  ************************************
     F*

     D*** BEGIN ADD ***  11/09/98  **************************************
     D CMD             S             80    DIM(1) CTDATA PERRCD(1)              Cmd for OVR
     D*** END ADD *****  11/09/98  **************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*** BEGIN ADD ***  11/09/98  **************************************
     D                 DS
     D  QQCMD                  1     80
     D  XXOVL                 26     33
     D*** END ADD *****  11/09/98  **************************************
     D*
     D QUOTED          C                   CONST('Q U O T E D')
     D ESTMTD          C                   CONST('E S T I M A T E D')
     D REVSD           C                   CONST('R E V I S E D')
     D AUDIT           C                   CONST('A U D I T')
     D NODUE           C                   CONST('NO INSTALLMENTS DUE -
     D                                     FOR THIS EMPLOYER')
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C*----------------------------------------------------------------
     C*  Key list definitions
     C*----------------------------------------------------------------
     C*    Employer master file
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C*
     C*    Employer detail file
     C     ELKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*** BEGIN ADD ***  11/09/98  ************************************
     C*    GROUP DETAIL FILE
     C     F2KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*** END ADD *****  11/09/98  ************************************
     C*** BEGIN ADD ****** 07/06/10 ************************************
     C     F1KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C*** END   ADD ****** 07/06/10 ************************************
     C*
     C*    Third address file
     C     A3KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*    Office file
     C     OFKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKOFC#
     C*
     C*    Table file
     C     TBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C*    A/R transaction file
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEMP#
     C*
     C     EAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKPODT
     C*
     C     EAKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKPODT
     C*
     C     EAKY3         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*  Output routing file
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C*  User parameter file
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*----------------------------------------------------------------
     C*  LIKE DEFINES
     C*----------------------------------------------------------------
     C     *LIKE         DEFINE    EMCO#         KKCO#                          key field
     C     *LIKE         DEFINE    EMFND         KKFND                              "
     C     *LIKE         DEFINE    EMEMP#        KKEMP#                             "
     C     *LIKE         DEFINE    ELFYR         KKFYR                              "
     C     *LIKE         DEFINE    EMDIV         KKDIV                              "
     C     *LIKE         DEFINE    EAPODT        KKPODT                             "
     C     *LIKE         DEFINE    BCKEY         KKKEY                              "
     C     *LIKE         DEFINE    OFOFC#        KKOFC#                             "
     C     *LIKE         DEFINE    EMDIV         XXSTDV                         state division
     C     *LIKE         DEFINE    EMMAD1        XXADR1                         employer addr field
     C     *LIKE         DEFINE    EMMAD2        XXADR2                            "      "     "
     C     *LIKE         DEFINE    EMMAD2        XXADR3                            "      "     "
     C     *LIKE         DEFINE    EMMCTY        XXCITY                            "      "     "
     C     *LIKE         DEFINE    EMMST         XXST                              "      "     "
     C     *LIKE         DEFINE    OFAAD1        XXAAD1                         acctg addr field
     C     *LIKE         DEFINE    OFAAD2        XXAAD2                           "    "     "
     C     *LIKE         DEFINE    OFAAD2        XXAAD3                           "    "     "
     C     *LIKE         DEFINE    BCDESC        XXCMT1                         comment description
     C     *LIKE         DEFINE    BCDESC        XXCMT2                            "         "
     C     *LIKE         DEFINE    BCDESC        XXCMT3                            "         "
     C     *LIKE         DEFINE    EAPODT        XXFPDT
     C     *LIKE         DEFINE    EAPODT        XXTPDT
     C*** BEGIN ADD ***  11/09/98  ************************************
     C     *LIKE         DEFINE    F2DCID        QQSDOC
     C*** END ADD *****  11/09/98  ************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDATE            8
     C                   PARM                    QQCOM1            2
     C                   PARM                    QQCOM2            2
     C                   PARM                    QQCOM3            2
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQCALL            1
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
     C*** BEG ADD *** 01/28/10 ****************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C****END ADD *** 01/28/10 ****************************************
     C                   MOVE      QQCO#         KKCO#
     C                   MOVE      QQFND         KKFND
     C                   MOVE      QQFYR         KKFYR
     C                   MOVE      QQEMP#        KKEMP#
     C                   MOVE      QQDATE        XXPODT            8 0
     C                   MOVE      QQDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKPODT
     C*** BEGIN ADD ****** 08/09/10 ************************************
     C                   MOVEL     SRDTO         X7DATE            7 0
     C*** END   ADD ****** 08/09/10 ************************************
     C*** BEGIN ADD ***  11/09/98  ************************************
     C                   EXSR      SR1ST
     C*** END ADD *****  11/09/98  ************************************
     C*
     C*  Init fields
     C                   Z-ADD     0             XXHNF            11 2          H & F amount
     C                   Z-ADD     0             XXIAMT           11 2          installment amt
     C                   Z-ADD     0             XXPMNT           11 2          payments
     C                   Z-ADD     0             XXPREM           11 2          premium
     C                   SETOFF                                       75        audited indicator
     C                   SETOFF                                       77        no installments due
     C     XXPODT        IFEQ      *ZERO
     C                   SETOFF                                       78
     C                   ELSE
     C                   SETON                                        78
     C                   ENDIF
     C*
     C*  Chain to the user parm file
     C     UPKY          CHAIN     PUP01                              60
     C*
     C*  Batch or demand mode
     C     QQMODE        IFEQ      'D'
     C                   MOVE      *ON           *IN09                          Demand mode
     C                   ELSE
     C                   MOVE      *ON           *IN08                          Batch mode
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*
     C*  Convert system date to 8 digit schedule date
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXDATE            8 0
     C*
     C*  Get comment descriptions
     C                   MOVEL(P)  QQCOM1        KKKEY
     C     TBKY          CHAIN     PBC01                              71
     C     *IN71         IFEQ      *OFF
     C                   MOVEL     BCDESC        XXCMT1
     C                   ENDIF
     C*
     C                   MOVEL(P)  QQCOM2        KKKEY
     C     TBKY          CHAIN     PBC01                              72
     C     *IN72         IFEQ      *OFF
     C                   MOVEL     BCDESC        XXCMT2
     C                   ENDIF
     C*
     C                   MOVEL(P)  QQCOM3        KKKEY
     C     TBKY          CHAIN     PBC01                              73
     C     *IN73         IFEQ      *OFF
     C                   MOVEL     BCDESC        XXCMT3
     C                   ENDIF
     C*
     C*  Get the distribution information and print header
     C                   MOVE      '0'           XXPRT             1
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      *OFF
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   ENDIF
     C                   MOVE      *ON           *INOA
     C*
     C*  Get state reporting div for employer
     C     EMKY          SETLL     PEM01
     C     *IN60         DOUEQ     *ON
     C     EMSTRP        OREQ      'Y'
     C     EMKY          READE     PEM01                                  60
     C                   ENDDO
     C*
     C                   MOVE      EMDIV         XXSTDV                         state division
     C                   MOVE      EMDIV         KKDIV
     C*
     C*  Get employer detail record
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*    Get policy from/to dates
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDT            8 0
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODT            8 0
     C*
     C*    Load policy number
     C                   CALL      'WA022R'
     C                   PARM      *BLANKS       XXPLCY           15
     C                   PARM                    KKEMP#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C*
     C*    Determine report title
     C                   SELECT
     C     QQCALL        WHENEQ    'Q'
     C                   MOVE      QUOTED        XXTITL           17
     C*
     C     ELFAF         WHENEQ    'E'
     C                   MOVE      ESTMTD        XXTITL
     C*
     C     ELFAF         WHENEQ    'R'
     C                   MOVE      REVSD         XXTITL
     C*
     C     ELFAF         WHENEQ    'A'
     C                   MOVE      AUDIT         XXTITL
     C                   SETON                                        75
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  Get office information for employer
     C                   MOVE      EMOFF#        KKOFC#
     C     OFKY          CHAIN     POF01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     OFAAD1        XXAAD1
     C                   MOVEL     OFACTY        XXCITY
     C                   MOVE      OFAST         XXST
     C                   MOVEL     OFAZIP        XXZIP5            5
     C                   MOVE      OFAZIP        XXZIP4            4
     C*
     C                   EXSR      SRCSZ
     C*
     C     OFAAD2        IFNE      *BLANK
     C                   MOVEL     OFAAD2        XXAAD2
     C                   MOVEL     XXCSZ         XXAAD3
     C                   ELSE
     C                   MOVEL     XXCSZ         XXAAD2
     C                   MOVEL     *BLANK        XXAAD3
     C                   ENDIF                                                  OFAAD2<>*BLANK
     C*
      ***** BEG ADD **** 02/13/03 *************************************
      ***** BEG DEL **** 05/08/03 *************************************
      ***If EXISTS use LOCK BOX address instead of Accounting Ofc addr
     C*******    XXLKBX    IFEQ 'Y'
     C*******              MOVELF2LAD1    XXAAD1    P
     C*******              MOVELF2LAD2    XXAAD2    P
     C*******              MOVELF2LCTY    XXCITY
     C*******              MOVELF2LST     XXST
     C*******              MOVELF2LZIP    XXZIP1  5
     C*******              MOVE F2LZIP    XXZIP2  4
     C*******              EXSR SRCSZ
     C*******              MOVELXXCSZ     XXAAD3
     C*******    F2LAD2    IFEQ *BLANK
     C*******              MOVELXXAAD3    XXAAD2
     C*******              MOVEL*BLANK    XXAAD3
     C*******              ENDIF
     C*******              ENDIF
      ***** END DEL **** 05/08/03 *************************************
      ***** END ADD **** 02/13/03 *************************************
     **
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  Set indicator if EMFDNM is not blank
     C     EMFDNM        IFNE      *BLANK
     C                   SETON                                        76
     C                   ENDIF
     C*
     C*  Call subroutine to load employer address fields
     C                   EXSR      SRADDR
     C*
     C*
     C*  Accumulate payments and 'H' and 'F' transactions across all divs.
     C*  Also total the premium.
     c*
     C     EAKY          CHAIN     PEA01                              60
B01  C     *IN60         DOWEQ     *OFF
     C*
     C                   SELECT
     C     EAARTY        WHENEQ    'P'
     C*** BEG ADD ***** 10/06/10 ***************************************
     C     EAARCD        ANDNE     F1RCCD
     C*** END ADD ***** 10/06/10 ***************************************
     C     EAARTY        OREQ      'I'
     C     EAARTY        OREQ      'Q'
     C***  BEGIN DEL  ***  01/24/00  *********************************
     C********** EAARTY    OREQ 'V'
     C***  END   DEL  ***  01/24/00  *********************************
     C***  BEG ADD ******  02/19/04  *********************************
     C     EAARTY        OREQ      'T'
     C***  END ADD ******  02/19/04  *********************************
     C                   ADD       EAAMT         XXPMNT                         payments
     C*
     C     EAARTY        WHENEQ    'R'
     C***  BEGIN ADD  ***  01/24/00  *********************************
     C     EAARTY        OREQ      'V'
     C***  END   ADD  ***  01/24/00  *********************************
     C                   SUB       EAAMT         XXPMNT                         refunds
     C*
     C     EAARTY        WHENEQ    'H'
     C                   ADD       EAAMT         XXHNF                          H & F
     C*
     C     EAARTY        WHENEQ    'F'
     C                   MOVEL(P)  EAARCD        KKKEY
     C     TBKY          CHAIN     PTT01                              60
     C     TTCTYP        IFEQ      'T'                                          <add amt for
     C     TTCTYP        OREQ      '1'                                          <these categories
     C     TTCTYP        OREQ      '2'                                          <to premium
     C*** BEGIN ADD ***  01/02/01  ************************************
     C     TTCTYP        OREQ      '3'                                          <to premium
     C*** END ADD *****  01/02/01  ************************************
     C*** BEG ADD *****  07/02/04  ************************************
     C     TTCTYP        OREQ      '4'                                          <to premium
     C     TTCTYP        OREQ      '5'                                          <to premium
     C*** END ADD *****  07/02/04  ************************************
     C***  Begin Add  ***  12/11/08  **************************************
     C     TTCTYP        OREQ      '6'                                          <to premium
     C     TTCTYP        OREQ      '7'                                          <to premium
     C***  End   Add  ***  12/11/08  **************************************
     C                   ADD       EAAMT         XXPREM
     C                   ELSE
     C                   ADD       EAAMT         XXHNF
     C                   ENDIF                                                  TTCTYP=T,1,2
     C*
     C     EAARTY        WHENEQ    'B'                                          <
     C     EAARTY        OREQ      'A'                                          <check for valid
     C     EAARTY        OREQ      'D'                                          <installment trans
     C     EADIV         IFEQ      XXSTDV                                       <and set indicator
     C                   SETON                                        99        <when found
     C                   ENDIF                                                  <
     C                   ADD       EAAMT         XXPREM
     C*
     C                   ENDSL
     C*
     C     EAKY          READE     PEA01                                  60
     C*
E01  C                   ENDDO                                                  *IN60=*OFF
     C*
     C*  Print out header info
     C                   EXCEPT    HEAD1
     C*
     C*  Find the first 'B' 'A' or 'D' transaction with a posting date
     C*  greater than or equal than the installment posting date passed
     C*  in to the program.
     C     EAKY2         SETLL     PEA08
B02  C     *IN60         DOUEQ     *ON
     C     EAARTY        OREQ      'B'
     C     EAARTY        OREQ      'A'
     C     EAARTY        OREQ      'D'
     C     EAKY3         READE     PEA08                                  60
E02  C                   ENDDO                                                  *IN60=*ON
     C*
     C*  Process installment date
     C                   Z-ADD     0             XXFPDT
     C*
B03  C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     EAPODT        XXTPDT
     C*
     C                   EXSR      SRCALC
     C*
     C*    Get the next valid installment date
     C                   Z-ADD     XXTPDT        XXFPDT
     C                   Z-ADD     XXTPDT        KKPODT
     C*
     C     EAKY2         SETGT     PEA08
B04  C     *IN60         DOUEQ     *ON
     C     EAARTY        OREQ      'B'
     C     EAARTY        OREQ      'A'
     C     EAARTY        OREQ      'D'
     C     EAKY3         READE     PEA08                                  60
E04  C                   ENDDO                                                  *IN60=*ON
     C*
E03  C                   ENDDO                                                  *IN60*OFF
     C*
     C*  Print msg if no balance on last installment
     C     XXIAMT        IFEQ      *ZERO
     C                   SETON                                        77
     C                   ENDIF
     C*
     C*  Print comment lines if comment codes entered
     C                   EXCEPT    COMM
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       If %open(qsysprt);
       xxPrt = '0';
       Chain ORKY POR01;
          If %found;
             Except Route;
          Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*** BEGIN ADD ***  11/09/98  ***************************************
     C                   CLOSE     QSYSPRT
     C*** END ADD *****  11/09/98  ***************************************
     C                   MOVE      *ON           *INLR
     C*
     C*================================================================
     C*  SRCSZ - Load the city, state, and zip to single field
     C*================================================================
     C     SRCSZ         BEGSR
     C*
     C     XXCITY        CAT(P)    ',':0         XXCSZ            40
     C                   CAT       XXST:1        XXCSZ
     C                   CAT       XXZIP5:1      XXCSZ
     C     XXZIP4        IFNE      *BLANK
     C     XXZIP4        ANDNE     '0000'
     C                   CAT       '-':0         XXCSZ
     C                   CAT       XXZIP4:0      XXCSZ
     C                   ENDIF
     C*
     C                   ENDSR                                                  SRCSZ
     C*================================================================
     C*  SRCALC - Calculates the installment amt and prints if not = 0
     C*================================================================
     C     SRCALC        BEGSR
     C*
     C                   Z-ADD     0             XXIAMT
     C*
     C*  If first installment add 'F' and 'H' amount
     C     XXFPDT        IFEQ      0
     C                   Z-ADD     XXHNF         XXIAMT
     C                   ENDIF                                                  XXFPDT=0
     C*
     C                   Z-ADD     XXFPDT        KKPODT
     C     EAKY1         SETGT     PEA01
     C     EAKY          READE     PEA01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     EAPODT        ANDLE     XXTPDT
     C*
     C     EAARTY        IFEQ      'B'
     C     EAARTY        OREQ      'A'
     C     EAARTY        OREQ      'D'
     C                   ADD       EAAMT         XXIAMT
     C                   ENDIF                                                  EAARTY='B'
     C*
      *** BEG ADD ***** 10/06/10 **********************************
     C     EAARCD        IFEQ      F1RCCD
     C                   SUB       EAAMT         XXIAMT
     C                   ENDIF
      *** END ADD ***** 10/06/10 **********************************
     C*
     C     EAARTY        IFEQ      'F'
     C                   MOVEL(P)  EAARCD        KKKEY
     C     TBKY          CHAIN     PTT01                              60
     C     TTCTYP        IFEQ      'T'
     C     TTCTYP        OREQ      '1'
     C     TTCTYP        OREQ      '2'
     C*** BEGIN ADD ***  01/02/01  ************************************
     C     TTCTYP        OREQ      '3'
     C*** END ADD *****  01/02/01  ************************************
     C*** BEG ADD *****  07/02/04  ************************************
     C     TTCTYP        OREQ      '4'
     C     TTCTYP        OREQ      '5'
     C*** END ADD *****  07/02/04  ************************************
     C***  Begin Add  ***  12/11/08  **************************************
     C     TTCTYP        OREQ      '6'
     C     TTCTYP        OREQ      '7'
     C***  End   Add  ***  12/11/08  **************************************
     C                   ADD       EAAMT         XXIAMT
     C                   ENDIF                                                  TTCTYP=T,1,2
     C                   ENDIF                                                  EAARTY='F'
     C*
     C     EAKY          READE     PEA01                                  60
     C                   ENDDO
     C*
     C*  Dissolve payments over amount
     C     XXPMNT        IFGE      XXIAMT
     C                   SUB       XXIAMT        XXPMNT
     C                   Z-ADD     0             XXIAMT
     C                   ELSE
     C                   SUB       XXPMNT        XXIAMT
     C                   Z-ADD     0             XXPMNT
     C                   ENDIF                                                  XXPMNT>XXIAMT
     C*
     C*  Print if installment amount is greater than 0
     C     XXIAMT        IFGT      0
     C*
     C                   Z-ADD     XXTPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXIDT             8 0
     C*
     C                   EXCEPT    DETL
     C*
     C                   ENDIF                                                  XXIAMT>0
     C*
     C                   ENDSR                                                  SRCALC
     C*================================================================
     C*  SRADDR - Loads the employer address : either the billing,
     C*           mailing, or physical address selected based upon
     C*           non-blank entry in that order
     C*================================================================
     C     SRADDR        BEGSR
     C*
     C     A3KY          CHAIN     PA301                              60
     C     *IN60         IFEQ      *OFF
      ***** BEG ADD ******* 12/10/98 **********************************
     C     A3STAT        ANDEQ     'A'
      ***** END ADD ******* 12/10/98 **********************************
     C     A3ADR1        ANDNE     *BLANK
     C*
     C                   MOVEL(P)  A3ADR1        XXADR1
     C                   MOVEL(P)  A3ADR2        XXADR2
     C                   MOVEL(P)  A3CTY         XXCITY
     C                   MOVE      A3ST          XXST
     C                   MOVE      A3ZIP1        XXZIP5
     C                   MOVE      A3ZIP2        XXZIP4
     C*
     C                   ELSE
     C*
     C     EMMAD1        IFNE      *BLANK
     C                   MOVEL(P)  EMMAD1        XXADR1
     C                   MOVEL(P)  EMMAD2        XXADR2
     C                   MOVEL(P)  EMMCTY        XXCITY
     C                   MOVEL     EMMST         XXST
     C                   MOVEL     EMMZIP        XXZIP5
     C                   MOVE      EMMZIP        XXZIP4
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  EMPAD1        XXADR1
     C                   MOVEL(P)  EMPAD2        XXADR2
     C                   MOVEL(P)  EMPCTY        XXCITY
     C                   MOVE      EMPST         XXST
     C                   MOVEL     EMPZIP        XXZIP5
     C                   MOVE      EMPZIP        XXZIP4
     C*
     C                   ENDIF                                                  EMMAD1
     C*
     C                   ENDIF                                                  A3ADR1<>*BLANK
     C*
     C                   EXSR      SRCSZ
     C                   MOVEL     XXCSZ         XXADR3
     C*
     C     XXADR2        IFEQ      *BLANK
     C                   MOVEL     XXADR3        XXADR2
     C                   MOVE      *BLANK        XXADR3
     C                   ENDIF
     C*
     C*** BEGIN ADD ***  10/07/05  ************************************
     C                   MOVE      KKCO#         ##CO#             3
     C                   MOVE      KKFND         ##FND             3
     C                   MOVE      KKEMP#        ##EMP#            9
     C                   MOVE      KKDIV         ##DIV             5
     C                   MOVE      ' '           ##ODR             1
     C                   MOVE      ' '           ##NODR            1
     C                   MOVE      ' '           ##AODR            1
     C                   MOVE      *BLANKS       ##EMN1           50
     C                   MOVE      *BLANKS       ##EMN2           50
     C                   MOVE      *BLANKS       ##EMN3           50
     C                   MOVE      *BLANKS       ##EMN4           50
     C                   MOVE      *BLANKS       ##EMN5           50
     C                   MOVE      *BLANKS       ##EMN6           50
     C                   MOVE      *BLANKS       ##EMN7           50
     C                   CALL      'SRADDR'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##ODR
     C                   PARM                    ##NODR
     C                   PARM                    ##AODR
     C                   PARM                    ##EMN1
     C                   PARM                    ##EMN2
     C                   PARM                    ##EMN3
     C                   PARM                    ##EMN4
     C                   PARM                    ##EMN5
     C                   PARM                    ##EMN6
     C                   PARM                    ##EMN7
     C*
     C                   MOVE      ##EMN1        XXEMN1           50
     C                   MOVE      ##EMN2        XXEMN2           50
     C                   MOVE      ##EMN3        XXEMN3           50
     C                   MOVE      ##EMN4        XXEMN4           50
     C                   MOVEL     ##EMN5        XXEMN5           40
     C                   MOVEL     ##EMN6        XXEMN6           40
     C                   MOVEL     ##EMN7        XXEMN7           40
     C*** END ADD *****  10/07/05  ************************************
     C                   ENDSR                                                  SRADDR
     C*
     C*** BEGIN ADD ***  11/09/98  ***************************************
     C******************************************************************
     C*  OVERRIDE THE QSYSPRT FILE TO HAVE A FRONTOVL OF QQSDOC(FROM F2 FIL)
     C******************************************************************
     C     SR1ST         BEGSR
     C*
     C*** BEGIN ADD ****** 08/09/10 ************************************
     C     F1KY          CHAIN     PF101                              60
     C*** END   ADD ****** 08/09/10 ************************************
     C     F2KY          CHAIN     PF201                              60
     **
      ***** BEG ADD **** 02/13/03 *************************************
      ***** BEG DEL **** 05/08/03 *************************************
      ***Check for Lock box address in Group Auxiliary file
      ***If exists use lock box address instead of Accounting Ofc addr
     C********             MOVE ' '       XXLKBX  1
     C**N60***   F2LAD1    IFGT *BLANKS
     C********             MOVE 'Y'       XXLKBX
     C********             ENDIF
      ***** END DEL **** 05/08/03 *************************************
      ***** END ADD **** 02/13/03 *************************************
     C*
     C     *IN60         IFEQ      *OFF
     C     F2DCID        ANDNE     *BLANKS
     C*
     C                   MOVEL     F2DCID        QQSDOC
     C                   Z-ADD     80            QQLEN
     C                   MOVEL     CMD(1)        QQCMD
     C                   MOVEL     QQSDOC        XXOVL
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD            80
     C                   PARM                    QQLEN            15 5
     C                   MOVE      '1'           OVRVAR            1
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   OPEN      QSYSPRT
     C*
     C                   ENDSR
     C******************************************************************
     C*** END ADD *****  11/09/98  ***************************************
     O*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
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
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          1
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
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O**   SELECTION CRITERIA
     O          E            ROUTE          1
     O                                           20 'COMPANY      :'
     O                       KKCO#         Z     25
     O          E            ROUTE          1
     O                                           20 'GROUP        :'
     O                       KKFND         Z     25
     O          E            ROUTE          1
     O                                           20 'POLICY YEAR  :'
     O                       KKFYR         Z     25
     O          E            ROUTE          1
     O                                           20 'EMPLOYER     :'
     O                       KKEMP#        Z     31
     O          E            ROUTE          1
     O                                           20 'INSTALLMNT DT:'
     O                       XXPODT              32 '  /  /    '
     O          E            ROUTE          1
     O                                           20 'COMMENT 1    :'
     O                       QQCOM1              24
     O                       XXCMT1            +002
     O          E            ROUTE          1
     O                                           20 'COMMENT 2    :'
     O                       QQCOM2              24
     O                       XXCMT2            +002
     O          E            ROUTE          1
     O                                           20 'COMMENT 3    :'
     O                       QQCOM3              24
     O                       XXCMT3            +002
      ****** BEG CHG ***** 01/28/10 **********************************
     O******  E  160           ROUTE
     O******                             23 '(C) COPYRIGHT PROGRESSI'
     O******                           +000 'VE DATA SOLUTIONS, INC.'
     O******                   *YEAR   +001
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O*** BEGIN CHG ***  11/09/98  ************************************
     O*****QSYSPRT E  106           HEAD1
     O*****                    XXTITL    26
     O*****                              53 'I N S T A L L M E N T  '
     O*****                              70 'S C H E D U L E'
     O*****   E  2             HEAD1
     OQSYSPRT   E            HEAD1          2 04
     O                       XXTITL              31
     O                                           57 'I N S T A L L M E N T  '
     O                                           73 'S C H E D U L E'
     O          E            HEAD1       2  2
     O*** END CHG *****  11/09/98  ************************************
     O                                           24 '________________________'
     O                                           48 '________________________'
     O                                           72 '________________________'
     O                                           80 '________'
     O          E            HEAD1          1
     O                       OFACNM              40
     O                                           56 'Schedule date :'
     O                       XXDATE              67 '  /  /    '
     O          E            HEAD1          1
     O                       XXAAD1              40
     O               78                          56 'Posting date  :'
     O               78      XXPODT              67 '  /  /    '
     O              N78                          56 'Policy #      :'
     O              N78      XXPLCY              72
     O          E            HEAD1          1
     O                       XXAAD2              40
     O               78                          56 'Policy #      :'
     O               78      XXPLCY              72
     O              N78                          56 'Policy period :'
     O              N78      XXFRDT              67 '  /  /    '
     O              N78                          69 '-'
     O              N78      XXTODT              80 '  /  /    '
     O*** BEGIN CHG ***  10/07/05  ************************************
     O******* E  3             HEAD1
     O          E            HEAD1          2
     O*** END CHG *****  10/07/05  ************************************
     O                       XXAAD3              40
     O               78                          56 'Policy period :'
     O               78      XXFRDT              67 '  /  /    '
     O               78                          69 '-'
     O               78      XXTODT              80 '  /  /    '
     O*** BEGIN CHG ***  10/07/05  ************************************
     O******* E  1             HEAD1
     O*******                  EMNAME    40
     O*******         N75                56 'Annual premium:'
     O*******          75                56 'Audit premium :'
     O*******                  XXPREMJ   71
     O******* E  1     76      HEAD1
     O*******                  EMFDNM    40
     O******* E  1             HEAD1
     O*******                  XXADR1    40
     O******* E  1             HEAD1
     O*******                  XXADR2    40
     O******* E  1             HEAD1
     O*******                  XXADR3    40
     O          E            HEAD1          1
     O              N75                          56 'Annual premium:'
     O               75                          56 'Audit premium :'
     O                       XXPREM        J     71
     O          E            HEAD1          1
     O                       XXEMN1              50
     O          E            HEAD1          1
     O                       XXEMN2              50
     O          E            HEAD1          1
     O                       XXEMN3              50
     O          E            HEAD1          1
     O                       XXEMN4              50
     O          E            HEAD1          1
     O                       XXEMN5              40
     O          E            HEAD1          1
     O                       XXEMN6              40
     O          E            HEAD1          1
     O                       XXEMN7              40
     O*** END CHG *****  10/07/05  ************************************
     O          E            HEAD1          1
     O                                           24 '________________________'
     O                                           48 '________________________'
     O                                           72 '________________________'
     O                                           80 '________'
     O          E            HEAD1          0
     O                                           17 'Installments Due'
     O                                           37 'Amount'
     O          E            HEAD1          2
     O                                           24 '________________________'
     O                                           48 '________________________'
     O                                           72 '________________________'
     O                                           80 '________'
     O          E            DETL           1
     O                       XXIDT               15 '  /  /    '
     O                       XXIAMT        1     37
     O          E    77      COMM           1
     O                       NODUE               58
     O          E            COMM        1  2
     O                                           24 '________________________'
     O                                           48 '________________________'
     O                                           72 '________________________'
     O                                           80 '________'
     O          E   N71      COMM           1
     O                       XXCMT1
     O          E   N72      COMM           1
     O                       XXCMT2
     O          E   N73      COMM           1
     O                       XXCMT3
**    CMD FOR OVRPRTF OVERLAY NAME
OVRPRTF QSYSPRT FRONTOVL(XXXXXXXX)
