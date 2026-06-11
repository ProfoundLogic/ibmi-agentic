     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H/TITLE W0070R50 - Payment Search Prompt/Call Search pgm
     H*****************************************************************
     H* DESCRIPTION:                                                  *
     H*                                                               *
     H*   This program will display a prompt screen for Payment Search*
     H*   Type and available subset parameters.  The user will be     *
     H*   able to execute searches for:  1) Request for Payments,     *
     H*   2) Paid Payments, and 3) Managed Care Bill Entry Detail.    *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W0070R - Work With Claims                                   *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   W0583R - Work With Requested Payments                       *
     H*   W0576R - Work With Paid Payments                            *
     H*   ______ - Managed Care Bill Entry Detail                     *
     H*                                                               *
     H* CREATED:  06/03/97  C:30  Project:  WC660                     *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/28/11  C:257  SUPPORT: 77403                               *
     H*                  Call F0350R with new value in parameter so   *
     H*                  that option '21' can be disabled             *
     H*                                                               *
     F* 01/28/10  C:249  Project: SY215                               *
     F*                  Retrieve brand Name
     F*                                                               *
     H*  06/14/02 C:15  Support: 33008/WC048                          *
     H*                 Changed modifier entry to alpha to pgm can    *
     H*                 differentiate between 0 and blank.            *
     H*                                                               *
     H*  01/19/00 C:53  PROJECT: WC025 25829                          *
     H*                 ADDED MODIFIER TO PAYMENT PARMS               *
     H*                                                               *
     H*  07/17/00 C:62  PROJECT: MC230                                *
     H*                 CALL TO F0350R NEEDS ##FYR AND ##CASE TO BE   *
     H*                 POPULATED.                                    *
     H*                                                               *
     H*  06/12/00 C:62  PROJECT: MC230                                *
     H*                 SWAP OUT FA015R WITH FA700R.                  *
     H*                                                               *
     H*  12/06/99 C:15  Support: WC044/20453                          *
     H*                 Added code to check for recursive calls to    *
     H*                 the WCMC pgms.                                *
     H*                                                               *
     H*  11/29/99 C:118 PROJECT: WC577
     H*                 Made V3 display changes.
     H*
     H*  04/05/99 C:31  Support: 17384
     H*                 F0350R Decimal Data Error
     H*                 1. Condition selection of F0350R.
     H*                 2. Add selection of F0349R.
     H*
     H*  01/11/99 C:31  Project: MC152
     H*                 V4: Bill Entry re-write
     H*                 1. Complete option 21 support
     H*                 2. Restrict type of search - 3
     H*                                                               *
     H*  01/08/98 C:15 Project: WC577                                 *
     H*               -Do not display or allow the type '3' Managed   *
     H*                Care search if WCMC is not installed.          *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H* N20 - WCMC is installed and interfaced.                       *
     H*  20 - WCMC is not installed or interfaced.                    *
     H*****************************************************************
      * Display file
     FW0070F50  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      *________________________________________________________________
      * Table file
     FWTABL     IF   E           K DISK
      *________________________________________________________________
      * User Parameters file
     FWMUPL     IF   E           K DISK
      *________________________________________________________________
      * Payment Status file
     FWTPSL     IF   E           K DISK
      *________________________________________________________________
      *
      /COPY SCOPYPR01D
     D I6              S              6    DIM(63)                              Indicator Values
      *
     D*** BEGIN ADD *** 01/28/10 **************************************
     D*
     D***PY SCOPYPR01I
     D*
     D*** END ADD ***** 01/28/10 **************************************
      *  Payment Status
      *
     D                 DS
     D  PS                     1     10
     D                                     DIM(5)                               Payment Status
     D  SCPPS1                 1      2
     D  SCPPS2                 3      4
     D  SCPPS3                 5      6
     D  SCPPS4                 7      8
     D  SCPPS5                 9     10
      *
      *  Payment Codes
      *
     D                 DS
     D  PC                     1     10
     D                                     DIM(5)                               Payment Codes
     D  SCPPC1                 1      2
     D  SCPPC2                 3      4
     D  SCPPC3                 5      6
     D  SCPPC4                 7      8
     D  SCPPC5                 9     10
      *
      *  Transaction Type
      *
     D                 DS
     D  TT                     1      5
     D                                     DIM(5)                               Transaction Type
     D  SCPTT1                 1      1
     D  SCPTT2                 2      2
     D  SCPTT3                 3      3
     D  SCPTT4                 4      4
     D  SCPTT5                 5      5
      *
      *  Payment Value 1
      *
     D                 DS
     D  PV1                    1     10
     D                                     DIM(5)                               Payment Value 1
     D  SCPPV1                 1      2
     D  SCPPV2                 3      4
     D  SCPPV3                 5      6
     D  SCPPV4                 7      8
     D  SCPPV5                 9     10
      *
      *  Reserve Codes
      *
     D                 DS
     D  RC                     1     10
     D                                     DIM(5)                               Reserve Codes
     D  SCPRC1                 1      2
     D  SCPRC2                 3      4
     D  SCPRC3                 5      6
     D  SCPRC4                 7      8
     D  SCPRC5                 9     10
      *
     D*** BEGIN DEL *** 01/28/10 ********************************      ******
     D***         DS
     D***                                     1  120DSDTTM
     D***                                     1   60DSTIME
     D***                                     7  120DSDATE
     D*** END DEL ***** 01/28/10 ********************************      ******
      *
     D                 DS
     D  FIELD                  1     10
     D  FLDNAM                 1      5
     D  FLDNUM                 6      6  0
      *
     D CPRDS           C                   CONST('     Payment Related-
     D                                      Searches Prompt')
     D*** BEGIN DEL *** 01/28/10 ********************************      ******
     D***           'Progressive Data Sol-C         PDSNAM
     D***           'utions'
     D*** END DEL ***** 01/28/10 ********************************      ******
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
      *

     I*IDS01    E DSSZQ2P
     I*** BEGIN DEL *** 01/28/10 ********************************      ******
     I*** PSDS      ESDSSZQ1P
     I*** END DEL ***** 01/28/10 ********************************      ******
      *
      * ===================================================================
      *   File Key Lists
      * ===================================================================
      *
      *  Table File Key List
      *

     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
      *
      *  USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *  PLIST for "Payment Detail" & "Payment Request"
      *
     C     PAYMNT        PLIST
     C     *INKC         PARM                    ##KC
     C                   PARM                    ##KL
     C                   PARM                    @@CALL            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@SPS            10            Payment Status
     C                   PARM                    @@SFDT            8            Service "From" DT
     C                   PARM                    @@STDT            8            Service "To" DT
     C                   PARM                    @@SPC            10            Payment Codes
     C                   PARM                    @@STT             5            Transaction Type
     C                   PARM                    @@SPV1           10            Payment Value 1
     C                   PARM                    @@SRC            10            Reserve Codes
     C                   PARM                    @@SFEI            9            Federal ID #
     C                   PARM                    @@SCHK            7            Check #
     C                   PARM                    @@SEOB            9            EOMB #
     C                   PARM                    @@SIRQ           10            Initial Requestor
     C                   PARM                    @@SGRP            1            Group vs. Non-Group
     C*** BEG ADD *** 01/19/01 **************************************  *********
     C                   PARM                    @@SMOD            3            modifier
     C*** END ADD *** 01/19/01 **************************************  *********
     C*
     C*** BEGIN ADD *** 01/16/98 ***********************************************
     C*  PLIST for WCMC "Work with EOMBs"
     C*
     C*** BEGIN CHG *** 04/05/99 **************************************
     C********   WCMCPL    PLIST
     C     WCMCP1        PLIST
     C********             PARM           ##CALL
     C                   PARM                    @1CALL
     C*** END CHG *** 04/05/99 ****************************************
     C*** BEGIN CHG *** 06/12/00 **************************************
     C**********           PARM           ##CO#
     C**********           PARM           ##SUB#
     C**********           PARM           ##GRP#
     C**********           PARM           ##FYR
     C**********           PARM           ##CASE
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##GRP#            3 0
     C                   PARM                    ##FYR             3 0
     C                   PARM                    ##CASE            7 0
     C*** END   CHG *** 06/12/00 **************************************
     C                   PARM                    ##RTCD
     C*** BEGIN ADD *** 04/05/99 **************************************
     C     WCMCP2        PLIST
     C                   PARM                    @1CALL
     C                   PARM                    ##SUB#
     C                   PARM                    ##GRP#
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM                    ##EOMB            9 0
     C                   PARM                    @@CERT           15
     C*** END ADD *** 04/05/99 ****************************************
     C*** END ADD ***** 01/16/98 ***********************************************
     C*** BEGIN ADD *** 01/11/99 **************************************
     C                   PARM                    @@SFDT            8
     C                   PARM                    @@STDT            8
     C                   PARM                    @@SFEI            9
     C                   PARM                    @@SMOD            3
     C*** END ADD *** 01/11/99 ****************************************
      * ===================================================================
      *   PARAMETER LIST FOR PROGRAM
      * ===================================================================
      *
     C     *ENTRY        PLIST
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@CALL            1
     C*** BEGIN ADD *** 01/11/99 **************************************
     C                   PARM                    @@EOMB            9
     C*** END ADD *** 01/11/99 ****************************************
      *
     C/COPY SCOPYPR01C
      *
      ********************************************************************
      *  DSCAPI - Call API for getting descriptions
      ********************************************************************
     C     DSCAPI        BEGSR
      *
     C                   ENDSR
      ********************************************************************
      *  OTHFKS -
      ********************************************************************
     C     OTHFKS        BEGSR
      *
     C                   ENDSR
      *
      ********************************************************************
      *  ERRCK1 - Routine to Edit User Data Entry
      ********************************************************************
     C     ERRCK1        BEGSR
      *
     C     *INKJ         IFEQ      *ON                                          F10 Search
     C                   EXSR      KJDO
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
      *
      *  Edit 'Type of Search'
      *
     C     SCPTYP        IFNE      '1'
     C     SCPTYP        ANDNE     '2'
     C     SCPTYP        ANDNE     '3'
     C*** BEGIN ADD *** 01/08/98 ***********************************************
     C     *IN20         ANDEQ     '0'
     C     *IN20         OREQ      '1'
     C     SCPTYP        ANDNE     '1'
     C     SCPTYP        ANDNE     '2'
     C*** END ADD ***** 01/08/98 ***********************************************
     C                   SETON                                        9082
     C                   ENDIF
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 12/06/99 **************************************
     C*  Check for a recursive call.
     C*
     C     SCPTYP        IFEQ      '3'
     C     @@PRVB        ANDEQ     'Y'
     C                   MOVEL(P)  'F0350R'      ##PGM
     C                   CALL      'SRCHKPGM'
     C                   PARM                    ##PGM            10
     C                   PARM                    ##ERR             1
     C     ##ERR         IFEQ      *ON
     C                   SETON                                        9083
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCPTYP        IFEQ      '3'
     C     @@PRVB        ANDNE     'Y'
     C                   MOVEL(P)  'F0349R'      ##PGM
     C                   CALL      'SRCHKPGM'
     C                   PARM                    ##PGM
     C                   PARM                    ##ERR
     C     ##ERR         IFEQ      *ON
     C                   SETON                                        9083
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ***** 12/06/99 **************************************
      *
      *  Edit 'Payment Status' if entered
      *
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     1             X                 3 0
     C                   Z-ADD     *ZERO         STR               3 0
     C                   Z-ADD     1             Y                 3 0
     C                   DO        5             X
     C     PS(X)         IFNE      *BLANK
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     PS(X)         XXKEY
     C     TBKEY         CHAIN     PPS01                              60
     C     *IN60         IFEQ      *ON
     C     X             ADD       STR           Y
     C                   MOVEA     I6(Y)         CH6               6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
      *
      *  Edit Service Dates
      *
     C     SCPFDT        IFGT      *ZERO
     C     SCPTDT        ORGT      *ZERO
      *
      *  Edit "From" Service Date
      *
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPFDT
      *
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7790
     C                   ENDIF
      *
      *  Edit "TO" Date
      *
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPTDT
      *
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7890
     C                   END
      *
      *  "FROM" Date may not be greater than "TO" Date
      *
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFROM            8 0
      *
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTO              8 0
      *
     C     XXFROM        IFGT      XXTO
     C                   SETON                                        7990
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  Edit Service DT
      *
      *  Edit 'Payment Code' if entered
      *
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     1             X
     C                   Z-ADD     5             STR
     C                   Z-ADD     *ZERO         Y
     C                   DO        5             X
     C     PC(X)         IFNE      *BLANK
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     PC(X)         XXKEY
     C     TBKEY         CHAIN     PPC01                              60
     C     *IN60         IFEQ      *ON
     C     X             ADD       STR           Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
      *
      *  Edit 'Transaction Type' if entered
      *
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     1             X
     C                   Z-ADD     10            STR
     C                   Z-ADD     *ZERO         Y
     C                   DO        5             X
     C     TT(X)         IFNE      *BLANK
     C     TT(X)         ANDNE     'M'
     C     TT(X)         ANDNE     'A'
     C     TT(X)         ANDNE     'P'
     C     TT(X)         ANDNE     'R'
     C     TT(X)         ANDNE     'Z'
     C     TT(X)         ANDNE     'V'
     C     TT(X)         ANDNE     'S'
     C     X             ADD       STR           Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
      *
      *  Edit 'Payment Value 1' if entered
      *
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     1             X
     C                   Z-ADD     15            STR
     C                   Z-ADD     *ZERO         Y
     C                   DO        5             X
     C     PV1(X)        IFNE      *BLANK
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     PV1(X)        XXKEY
     C     TBKEY         CHAIN     PB201                              60
     C     *IN60         IFEQ      *ON
     C     X             ADD       STR           Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
      *
      *  Edit 'Reserve Code' if entered
      *
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     1             X
     C                   Z-ADD     20            STR
     C                   Z-ADD     *ZERO         Y
     C                   DO        5             X
     C     RC(X)         IFNE      *BLANK
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     RC(X)         XXKEY
     C     TBKEY         CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C     X             ADD       STR           Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
      *
      *  Edit 'Reserve Code' if entered
      *
     C     *IN90         IFEQ      *OFF
     C     SCPGRP        IFNE      *BLANK
     C     SCPGRP        ANDNE     'G'
     C                   SETON                                        8190
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR
      *
      *================================================================
      *  KJDO - Process F10 "Table Search" requests.                  *
      *================================================================
     C     KJDO          BEGSR
      *
     C                   MOVE      HFLD          FIELD
      *
      *  Assign Index references for ERROR Message or Position Cursor
      *  based on field requesting SEARCH.
      *
     C                   SELECT
      *
     C     FLDNAM        WHENEQ    'SCPPS'
     C                   MOVE      FLDNUM        KJNUM             3 0
     C                   Z-ADD     *ZERO         KJERR             3 0
     C                   Z-ADD     32            KJPC              3 0
      *
     C     FLDNAM        WHENEQ    'SCPPC'
     C                   MOVE      FLDNUM        KJNUM
     C                   Z-ADD     5             KJERR
     C                   Z-ADD     37            KJPC
      *
     C     FLDNAM        WHENEQ    'SCPTT'
     C                   MOVE      FLDNUM        KJNUM
     C                   Z-ADD     10            KJERR
     C                   Z-ADD     42            KJPC
      *
     C     FLDNAM        WHENEQ    'SCPPV'
     C                   MOVE      FLDNUM        KJNUM
     C                   Z-ADD     15            KJERR
     C                   Z-ADD     47            KJPC
      *
     C     FLDNAM        WHENEQ    'SCPRC'
     C                   MOVE      FLDNUM        KJNUM
     C                   Z-ADD     20            KJERR
     C                   Z-ADD     52            KJPC
      *
     C     HFLD          WHENEQ    'SCPTYP'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     1             KJPC
      *
     C     HFLD          WHENEQ    'SCPFDT'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     2             KJPC
      *
     C     HFLD          WHENEQ    'SCPTDT'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     3             KJPC
      *
     C     HFLD          WHENEQ    'SCPGRP'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     4             KJPC
      *
     C     HFLD          WHENEQ    'SCPFEI'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     5             KJPC
      *
     C     HFLD          WHENEQ    'SCPMOD'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     6             KJPC
      *
     C     HFLD          WHENEQ    'SCPCHK'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     7             KJPC
      *
     C     HFLD          WHENEQ    'SCPEOB'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     8             KJPC
      *
     C     HFLD          WHENEQ    'SCPIRQ'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     9             KJPC
      *
     C                   OTHER
     C                   Z-ADD     *ZERO         KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     *ZERO         KJPC
     C                   ENDSL
      *
      *  Select the field requesting SEARCH or return with Position Cursor.
      *
     C                   SELECT
      *
      *_________________________________________________________________
      *
     C     FLDNAM        WHENEQ    'SCPPS'
     C                   CALL      'W2142R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANK        ##KEY            10
      *
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C                   SETON                                        90
     C                   Z-ADD     FLDNUM        X
      *
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         PS(X)                          Payment Status
      *
     C     X             IFEQ      5
     C                   MOVEA     '0010'        *IN(86)                        Position Cursor
     C                   ELSE
     C     KJPC          ADD       X             Y                 3 0
     C                   ADD       1             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
      *
     C                   ELSE                                                   ##KEY Blank
     C     KJPC          ADD       X             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
     C                   ENDIF
      *_________________________________________________________________
      *
     C     FLDNAM        WHENEQ    'SCPPC'
     C                   CALL      'W2141R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANK        ##KEY            10
      *
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C                   SETON                                        90
     C                   Z-ADD     FLDNUM        X
      *
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         PC(X)                          Payment Status
      *
     C     X             IFEQ      5
     C                   MOVEA     I6(58)        CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ELSE
     C     KJPC          ADD       X             Y                 3 0
     C                   ADD       1             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
      *
     C                   ELSE                                                   ##KEY Blank
     C     KJPC          ADD       X             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
     C                   ENDIF
      *_________________________________________________________________
      *
     C     FLDNAM        WHENEQ    'SCPPV'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANK        ##KEY            10
      *
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C                   SETON                                        90
     C                   Z-ADD     FLDNUM        X
      *
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         PV1(X)                         Payment Status
      *
     C     X             IFEQ      5
     C                   MOVEA     I6(53)        CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ELSE
     C     KJPC          ADD       X             Y                 3 0
     C                   ADD       1             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
      *
     C                   ELSE                                                   ##KEY Blank
     C     KJPC          ADD       X             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
     C                   ENDIF
      *_________________________________________________________________
      *
     C     FLDNAM        WHENEQ    'SCPTT'
     C                   SETON                                        90
     C                   Z-ADD     FLDNUM        X
     C     KJPC          ADD       X             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
      *_________________________________________________________________
      *
     C     FLDNAM        WHENEQ    'SCPRC'
     C                   CALL      'W2136R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANK        ##KEY            10
      *
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C                   SETON                                        90
     C                   Z-ADD     FLDNUM        X
      *
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         RC(X)                          Payment Status
      *
     C     X             IFEQ      5
     C                   MOVEA     '0100'        *IN(86)                        Position Cursor
     C                   ELSE
     C     KJPC          ADD       X             Y                 3 0
     C                   ADD       1             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
      *
     C                   ELSE                                                   ##KEY Blank
     C     KJPC          ADD       X             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
     C                   ENDIF
      *_________________________________________________________________
      *
     C     HFLD          WHENEQ    'SCPFEI'
     C     HFLD          OREQ      'SCMOD'                                      Mod Search
     C                   MOVE      SCPFEI        QQFEIN
     C                   MOVE      SCPMOD        QQMOD
     C                   MOVE      '2'           QQCALL
     C                   CALL      'W0030R'
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQRTC             1
      *
     C                   MOVE      *OFF          *INKC
     C                   MOVE      *OFF          *INKL
      *
     C     QQRTC         IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
      *
     C     QQRTC         IFEQ      'B'
     C                   SETON                                        90
     C                   MOVEA     '0101'        *IN(86)
     C                   ENDIF
      *
     C     QQRTC         IFEQ      '*'
     C                   MOVE      QQFEIN        SCPFEI
     C                   MOVE      QQMOD         SCPMOD
     C                   SETON                                        90
     C                   MOVEA     '1010'        *IN(86)
     C                   ENDIF
      *_________________________________________________________________
     C     HFLD          WHENEQ    'SCPFDT'
     C                   SETON                                        90
     C                   MOVEA     '1011'        *IN(86)
      *_________________________________________________________________
     C     HFLD          WHENEQ    'SCPTDT'
     C                   SETON                                        90
     C                   MOVEA     '0011'        *IN(86)
      *_________________________________________________________________
     C     HFLD          WHENEQ    'SCPGRP'
     C                   SETON                                        90
     C                   MOVEA     '1100'        *IN(86)
      *_________________________________________________________________
     C     HFLD          WHENEQ    'SCPCHK'
     C                   SETON                                        90
     C                   MOVEA     '0111'        *IN(86)
      *_________________________________________________________________
     C     HFLD          WHENEQ    'SCPEOB'
     C                   SETON                                        90
     C                   MOVEA     '1000'        *IN(86)
      *_________________________________________________________________
     C     HFLD          WHENEQ    'SCPIRQ'
     C                   CALL      'SC530R'
     C                   PARM                    SCPIRQ
     C                   PARM                    QQRTCD            1
     C                   SETON                                        90
     C****                 MOVEA'1001'    *IN,86
      *_________________________________________________________________
      *
      *  NO SEARCH AVAILABLE
      *
     C                   OTHER
     C                   SETON                                        4990
     C     KJNUM         IFEQ      99
     C                   MOVE      I6(KJPC)      CH4               4
     C                   MOVEA     CH4           *IN(86)
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   ENDSR
      *
      ********************************************************************
      *  PGMEND - Routine prior to *INLR *ON
      ********************************************************************
     C     PGMEND        BEGSR
      *
     C                   ENDSR
      *
      ********************************************************************
      *  PGMSTR - Routine to Initialize program variables
      ********************************************************************
     C     PGMSTR        BEGSR
      *
     C/COPY SINDIC01
      *
      *   LOAD THE OUTPUT CONSTANTS FROM THE PARMS
      *
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVEL     CPRDS         SCPRDS
      *
      *  Access User Parameters
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   ENDIF
      *
     C                   MOVE      @@CO#         XXCO#             3 0
     C                   MOVE      @@FND         XXFND             3 0
     C                   MOVE      @@FYR         XXFYR             3 0
     C                   MOVE      @@CASE        XXCASE            7 0
      *
     C                   CALL      'WA006R'
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXFYR
     C                   PARM                    XXCASE
     C                   PARM                    SCCS01
     C                   PARM                    SCCS02
      *
     C*** BEGIN ADD *** 01/08/98 ***********************************************
     C*  Determine if WCMC is installed.
     C*
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C*** BEGIN ADD *** 04/05/99 **************************************
     C                   PARM                    @@AR              1            MC AR INST
     C                   PARM                    @@NTWK            1            NETWORK INST
     C                   PARM                    @@UR              1            PRECERT INST
     C                   PARM                    @@MED             1            MEDMGMT INST
     C                   PARM                    @@EDI             1            EDI INST
     C                   PARM                    @@WCDK            1            WC DARTS KEY
     C                   PARM                    @@RHAB            1            REHAB INST
     C                   PARM                    @@MSS             1            MS SUPPORT
     C                   PARM                    @@FSCH            1            FEE SCHEDULE
     C                   PARM                    @@PRVB            1            PROVIDER BILLS
     C*** END ADD *** 04/05/99 ****************************************
     C*
     C*  If WCMC is not installed or not interfaced with WCMS, do not
     C*    display/allow the type '3' Managed Care search.
     C*
     C     ##MCIF        IFNE      'Y'
     C     ##WCIN        ORNE      'Y'
     C                   MOVE      '1'           *IN20
     C                   ENDIF
     C*** END ADD ***** 01/08/98 ***********************************************
     C*
     C*** BEGIN ADD *** 01/16/98 ***********************************************
     C*  If WCMC is installed and interfaced, verify that a WCMC group
     C*  exists for the current WCMS group.
     C*
     C     *IN20         IFEQ      '0'
     C*
     C*** BEGIN CHG *** 06/12/00 **************************************
     C**********           CALL 'FA015R'
     C**********           PARM XXCO#     ##CO#   30
     C**********           PARM XXFND     ##FND   30
     C**********           PARM XXFYR     ##FYR   30
     C**********           PARM XXCASE    ##CASE  70
     C**********           PARM *ZEROS    ##EOB#  90
     C**********           PARM *ZEROS    ##RRN   90
     C**********           PARM *ZEROS    ##SUB#  50
     C**********           PARM *ZEROS    ##GRP#  30
     C**********           PARM *BLANK    ##RTCD  1
     C*
     C                   MOVE      XXCO#         #@CO#
     C                   MOVE      XXFND         #@WCF#
     C*** BEGIN ADD *** 07/17/00 **************************************
     C                   Z-ADD     XXFYR         ##FYR             3 0
     C                   Z-ADD     XXCASE        ##CASE            7 0
     C*** END   ADD *** 07/17/00 **************************************
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #@CO#             3
     C                   PARM                    #@WCF#            3
     C                   PARM      *BLANKS       #@SUB#            5
     C                   PARM      *BLANKS       #@FND             3
     C*** END   CHG *** 06/12/00 **************************************
     C*
     C     ##RTCD        IFEQ      'F'
     C                   MOVE      '1'           *IN20
     C*** BEGIN ADD *** 06/12/00 **************************************
     C                   ELSE
     C*
     C                   MOVE      #@SUB#        ##SUB#            5 0
     C                   MOVE      #@FND         ##GRP#            3 0
     C*** END   ADD *** 06/12/00 **************************************
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD ***** 01/16/98 ***********************************************
     C*** BEGIN ADD *** 01/11/99 **************************************
     C                   MOVE      @@EOMB        SCPEOB
     C     *IN20         IFEQ      *OFF
     C     @@EOMB        ANDNE     *BLANK
     C                   SETON                                        20
     C                   ENDIF                                                  *IN20 = Off
     C*** END ADD *** 01/11/99 ****************************************
      *
     C                   ENDSR
      *
      ********************************************************************
      *  PREDSP - Routine prior to Display Screen
      ********************************************************************
     C     PREDSP        BEGSR
      *
     C                   ENDSR
      *
      ********************************************************************
      *  PRSCTL - Routine to Call Selected Payment Search
      ********************************************************************
     C     PRSCTL        BEGSR
      *
      *  Assign Parameters
      *
     C                   MOVEA     PS            @@SPS                          Payment Status
     C                   MOVE      SCPFDT        @@SFDT                         Service From Date
     C                   MOVE      SCPTDT        @@STDT                         Service To Date
     C                   MOVEA     PC            @@SPC                          Payment Codes
     C                   MOVEA     TT            @@STT                          Transaction Type
     C                   MOVEA     PV1           @@SPV1                         Payment Value 1
     C                   MOVEA     RC            @@SRC                          Reserve Codes
     C                   MOVE      SCPFEI        @@SFEI                         Federal ID #
     C                   MOVE      SCPCHK        @@SCHK                         Check #
     C                   MOVE      SCPEOB        @@SEOB                         EOMB #
     C                   MOVE      SCPIRQ        @@SIRQ                         Initial Requestor
     C                   MOVE      SCPGRP        @@SGRP                         Group vs. Non-Group
     C*** BEG ADD *** 01/19/01 **************************************  *********
     C                   MOVE      SCPMOD        @@SMOD                         modifier
     C*** END ADD *** 01/19/01 **************************************  *********
      *
      *  Select Requested Search
      *
     C                   SELECT
      *
     C     SCPTYP        WHENEQ    '1'
     C                   CALL      'W0576R'      PAYMNT                 99
      *
     C     SCPTYP        WHENEQ    '2'
     C                   CALL      'W0583R'      PAYMNT                 99
      *
     C     SCPTYP        WHENEQ    '3'
     C*** BEGIN ADD *** 04/05/99 **************************************
     C*** BEGIN ADD *** 01/16/98 ***********************************************
     C********             CALL 'F0350R'  WCMCPL       99
     C*** END ADD ***** 01/16/98 ***********************************************
     C*** END ADD *** 04/05/99 ****************************************
     C*** BEGIN ADD *** 04/05/99 **************************************
     C                   MOVE      @@CALL        @1CALL            1
     C     @@PRVB        IFEQ      'Y'
     C*** BEGIN ADD *** 07/28/11 **************************************
     C                   MOVE      'X'           @1CALL
     C*** END   ADD *** 07/28/11 **************************************
     C                   Z-ADD     SCPEOB        ##EOMB            9 0
     C                   MOVEL(P)  SCPFDT        @@SFDT            8
     C                   MOVEL(P)  SCPTDT        @@STDT            8
     C                   MOVEL(P)  SCPFEI        @@SFEI            9
     C                   MOVEL(P)  SCPMOD        @@SMOD            3
     C                   CALL      'F0350R'      WCMCP2                 99
     C*** BEGIN ADD *** 07/28/11 **************************************
     C                   MOVE      @@CALL        @1CALL
     C*** END   ADD *** 07/28/11 **************************************
     C                   ELSE                                                   PRVB = 'Y'
     C                   CALL      'F0349R'      WCMCP1                 99
     C                   ENDIF                                                  PRVB = 'Y'
     C*** END ADD *** 04/05/99 ****************************************
      *
     C                   ENDSL
      *
      *  Assign *IN90 to *ON to allow SCR01 Re-Display
      *
     C                   MOVE      *ON           *IN90
      *
     C                   ENDSR
