     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H/TITLE W0070R92 - Payment Search - Exclude Hospital Program
     H*****************************************************************
     H* Description:                                                  *
     H*                                                               *
     H*   This program will display a prompt screen for Payment Search*
     H*   Type and available subset parameters.  The user will be     *
     H*   able to execute search for Paid Payments.                   *
     H*                                                               *
     H* Called From: W0070R - WW Claims                               *
     H* Calls to   : W0576R - WW Paid Payments                        *
     H*                                                               *
     H*****************************************************************
     H* Created on 12/26/2002 by c:62 for Project 114MFA              *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*                  Retrieve brand Name
     H*                                                               *
     H*                                                               *
     H*  08/18/06 C:53  PROJECT: 114WMF
     H*                 Back to base 114MFA - 12/26/02
     H*
     H*****************************************************************
     FW0070F92  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWTABL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWTPSL     IF   E           K DISK
      /COPY SCOPYPR01D
     D I6              S              6    DIM(63)                              Indicator Values
     D*****************************************************************
     D* Data Structures
     D*****************************************************************
     D*
     D*** BEGIN ADD *** 01/28/10 **************************************
     D*
     D***PY SCOPYPR01I
     D*
     D*** END ADD ***** 01/28/10 **************************************
     D*  Payment Status
     D*
     D                 DS
     D  PS                     1     10
     D                                     DIM(5)                               Payment Status
     D  SCPPS1                 1      2
     D  SCPPS2                 3      4
     D  SCPPS3                 5      6
     D  SCPPS4                 7      8
     D  SCPPS5                 9     10
     D*
     D*  Payment Codes
     D*
     D                 DS
     D  PC                     1     10
     D                                     DIM(5)                               Payment Codes
     D  SCPPC1                 1      2
     D  SCPPC2                 3      4
     D  SCPPC3                 5      6
     D  SCPPC4                 7      8
     D  SCPPC5                 9     10
     D*
     D*  Transaction Type
     D*
     D                 DS
     D  TT                     1      5
     D                                     DIM(5)                               Transaction Type
     D  SCPTT1                 1      1
     D  SCPTT2                 2      2
     D  SCPTT3                 3      3
     D  SCPTT4                 4      4
     D  SCPTT5                 5      5
     D*
     D*  Payment Value 1
     D*
     D                 DS
     D  PV1                    1     10
     D                                     DIM(5)                               Payment Value 1
     D  SCPPV1                 1      2
     D  SCPPV2                 3      4
     D  SCPPV3                 5      6
     D  SCPPV4                 7      8
     D  SCPPV5                 9     10
     D*
     D*  Reserve Codes
     D*
     D                 DS
     D  RC                     1     10
     D                                     DIM(5)                               Reserve Codes
     D  SCPRC1                 1      2
     D  SCPRC2                 3      4
     D  SCPRC3                 5      6
     D  SCPRC4                 7      8
     D  SCPRC5                 9     10
     D*
     D*** BEGIN DEL *** 01/28/10 ********************************      ******
     D***         DS
     D***                                     1  120DSDTTM
     D***                                     1   60DSTIME
     D***                                     7  120DSDATE
     D*** END DEL ***** 01/28/10 ********************************      ******
     D*
     D                 DS
     D  FIELD                  1     10
     D  FLDNAM                 1      5
     D  FLDNUM                 6      6  0
     D*****************************************************************
     D* Constants
     D*****************************************************************
     D CPRDS           C                   CONST('         Payment Hi-
     D                                     story Search    ')
     D*** BEGIN ADD *** 01/28/10 **************************************
     D***           'Progressive Data Sol-C         PDSNAM
     D***           'utions'
     D*** END ADD ***** 01/28/10 **************************************
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')

     I*
     I*FIDS01    E DSSZQ2P
     I*** BEGIN ADD *** 01/28/10 **************************************
     I***PSDS      ESDSSZQ1P
     I*** END ADD ***** 01/28/10 **************************************

     C*****************************************************************====
     C* Parameter Lists
     C*****************************************************************====
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@CALL            1
     C                   PARM                    @@EOMB            9
     C*
     C     PAYMNT        PLIST
     C     *INKC         PARM                    ##KC
     C                   PARM                    ##KL
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      @@CO#         ##CO#             3
     C                   PARM      @@FND         ##FND             3
     C                   PARM      @@FYR         #1FYR             3
     C                   PARM      @@CASE        #1CASE            7
     C                   PARM                    ##SPS            10            Payment Status
     C                   PARM                    ##SFDT            8            Service "From" DT
     C                   PARM                    ##STDT            8            Service "To" DT
     C                   PARM                    ##SPC            10            Payment Codes
     C                   PARM                    ##STT             5            Transaction Type
     C                   PARM                    ##SPV1           10            Payment Value 1
     C                   PARM                    ##SRC            10            Reserve Codes
     C                   PARM                    ##SFEI            9            Federal ID #
     C                   PARM                    ##SCHK            7            Check #
     C                   PARM                    ##SEOB            9            EOMB #
     C                   PARM      SCPIRQ        ##SIRQ           10            Initial Requestor
     C                   PARM                    ##SGRP            1            Group vs. Non-Group
     C                   PARM                    ##SMOD            3            modifier
     C                   PARM                    ##EXHS            1            exclude hospital
     C*
     C*****************************************************************====
     C* File Key Lists
     C*****************************************************************====
     C     TBKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    KKKEY
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*****************************************************************
     C* Like Defines
     C*****************************************************************
     C     *LIKE         DEFINE    PSKEY         KKKEY
     C*
     C/COPY SCOPYPR01C
     C********************************************************************
     C*  DSCAPI - Call API for getting descriptions
     C********************************************************************
     C     DSCAPI        BEGSR
     C*
     C*
     C                   ENDSR
      ********************************************************************
      *  OTHFKS -
      ********************************************************************
     C     OTHFKS        BEGSR
      *
     C                   ENDSR
     C********************************************************************
     C*  ERRCK1 - Routine to Edit User Data Entry
     C********************************************************************
     C     ERRCK1        BEGSR
     C*
     C     *INKJ         IFEQ      *ON                                          F10 Search
     C                   EXSR      KJDO
     C                   ENDIF
     C*
     C*  Edit 'Payment Status' if entered
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     1             X                 3 0
     C                   Z-ADD     *ZERO         STR               3 0
     C                   Z-ADD     1             Y                 3 0
     C                   DO        5             X
     C     PS(X)         IFNE      *BLANK
     C                   MOVEL(P)  PS(X)         KKKEY
     C     TBKY          CHAIN     PPS01                              99
     C     *IN99         IFEQ      *ON
     C     X             ADD       STR           Y
     C                   MOVEA     I6(Y)         CH6               6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C*
     C*  Edit Service Dates
     C*
     C     SCPFDT        IFGT      *ZERO
     C     SCPTDT        ORGT      *ZERO
     C*
     C*  Edit "From" Service Date
     C*
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7790
     C                   ENDIF
     C*
     C*  Edit "TO" Date
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPTDT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7890
     C                   END
     C*
     C*  "FROM" Date may not be greater than "TO" Date
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFROM            8 0
     C*
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTO              8 0
     C*
     C     XXFROM        IFGT      XXTO
     C                   SETON                                        7990
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  Edit Service DT
     C*
     C*  Edit 'Payment Code' if entered
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     1             X
     C                   Z-ADD     5             STR
     C                   Z-ADD     *ZERO         Y
     C                   DO        5             X
     C     PC(X)         IFNE      *BLANK
     C                   MOVEL(P)  PC(X)         KKKEY
     C     TBKY          CHAIN     PPC01                              99
     C     *IN99         IFEQ      *ON
     C     X             ADD       STR           Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C*
     C*  Edit 'Transaction Type' if entered
     C*
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
     C*
     C*  Edit 'Payment Value 1' if entered
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     1             X
     C                   Z-ADD     15            STR
     C                   Z-ADD     *ZERO         Y
     C                   DO        5             X
     C     PV1(X)        IFNE      *BLANK
     C                   MOVEL(P)  PV1(X)        KKKEY
     C     TBKY          CHAIN     PB201                              99
     C     *IN99         IFEQ      *ON
     C     X             ADD       STR           Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C*
     C*  Edit 'Reserve Code' if entered
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     1             X
     C                   Z-ADD     20            STR
     C                   Z-ADD     *ZERO         Y
     C                   DO        5             X
     C     RC(X)         IFNE      *BLANK
     C                   MOVEL(P)  RC(X)         KKKEY
     C     TBKY          CHAIN     PRT01                              99
     C     *IN99         IFEQ      *ON
     C     X             ADD       STR           Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C*
     C*  Edit 'Reserve Code' if entered
     C*
     C     *IN90         IFEQ      *OFF
     C     SCPGRP        IFNE      *BLANK
     C     SCPGRP        ANDNE     'G'
     C                   SETON                                        8190
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C*  KJDO - Process F10 "Table Search" requests.                  *
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   MOVE      HFLD          FIELD
     C*
     C*  Assign Index references for ERROR Message or Position Cursor
     C*  based on field requesting SEARCH.
     C*
     C                   SELECT
     C*
     C     FLDNAM        WHENEQ    'SCPPS'
     C                   MOVE      FLDNUM        KJNUM             3 0
     C                   Z-ADD     *ZERO         KJERR             3 0
     C                   Z-ADD     32            KJPC              3 0
     C*
     C     FLDNAM        WHENEQ    'SCPPC'
     C                   MOVE      FLDNUM        KJNUM
     C                   Z-ADD     5             KJERR
     C                   Z-ADD     37            KJPC
     C*
     C     FLDNAM        WHENEQ    'SCPTT'
     C                   MOVE      FLDNUM        KJNUM
     C                   Z-ADD     10            KJERR
     C                   Z-ADD     42            KJPC
     C*
     C     FLDNAM        WHENEQ    'SCPPV'
     C                   MOVE      FLDNUM        KJNUM
     C                   Z-ADD     15            KJERR
     C                   Z-ADD     47            KJPC
     C*
     C     FLDNAM        WHENEQ    'SCPRC'
     C                   MOVE      FLDNUM        KJNUM
     C                   Z-ADD     20            KJERR
     C                   Z-ADD     52            KJPC
     C*
     C     HFLD          WHENEQ    'SCPFDT'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     2             KJPC
     C*
     C     HFLD          WHENEQ    'SCPTDT'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     3             KJPC
     C*
     C     HFLD          WHENEQ    'SCPGRP'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     4             KJPC
     C*
     C     HFLD          WHENEQ    'SCPFEI'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     5             KJPC
     C*
     C     HFLD          WHENEQ    'SCPMOD'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     6             KJPC
     C*
     C     HFLD          WHENEQ    'SCPCHK'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     7             KJPC
     C*
     C     HFLD          WHENEQ    'SCPEOB'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     8             KJPC
     C*
     C     HFLD          WHENEQ    'SCPIRQ'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     9             KJPC
     C*
     C                   OTHER
     C                   Z-ADD     *ZERO         KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     *ZERO         KJPC
     C                   ENDSL
     C*
     C*  Select the field requesting SEARCH or return with Position Cursor.
     C*
     C                   SELECT
     C*
     C*----------------------------------------------------------------
     C*
     C     FLDNAM        WHENEQ    'SCPPS'
     C                   CALL      'W2142R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANK        ##KEY            10
     C*
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C                   SETON                                        90
     C                   Z-ADD     FLDNUM        X
     C*
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         PS(X)                          Payment Status
     C*
     C     X             IFEQ      5
     C                   MOVEA     '0010'        *IN(86)                        Position Cursor
     C                   ELSE
     C     KJPC          ADD       X             Y                 3 0
     C                   ADD       1             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
     C*
     C                   ELSE                                                   ##KEY Blank
     C     KJPC          ADD       X             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
     C                   ENDIF
     C*
     C*----------------------------------------------------------------
     C*
     C     FLDNAM        WHENEQ    'SCPPC'
     C                   CALL      'W2141R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANK        ##KEY            10
     C*
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C                   SETON                                        90
     C                   Z-ADD     FLDNUM        X
     C*
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         PC(X)                          Payment Status
     C*
     C     X             IFEQ      5
     C                   MOVEA     I6(58)        CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ELSE
     C     KJPC          ADD       X             Y                 3 0
     C                   ADD       1             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
     C*
     C                   ELSE                                                   ##KEY Blank
     C     KJPC          ADD       X             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
     C                   ENDIF
     C*                                                                _
     C*----------------------------------------------------------------
     C*
     C     FLDNAM        WHENEQ    'SCPPV'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANK        ##KEY            10
     C*
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C                   SETON                                        90
     C                   Z-ADD     FLDNUM        X
     C*
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         PV1(X)                         Payment Status
     C*
     C     X             IFEQ      5
     C                   MOVEA     I6(53)        CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ELSE
     C     KJPC          ADD       X             Y                 3 0
     C                   ADD       1             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
     C*
     C                   ELSE                                                   ##KEY Blank
     C     KJPC          ADD       X             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
     C                   ENDIF
     C*
     C*----------------------------------------------------------------_
     C*
     C     FLDNAM        WHENEQ    'SCPTT'
     C                   SETON                                        90
     C                   Z-ADD     FLDNUM        X
     C     KJPC          ADD       X             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C*
     C*----------------------------------------------------------------_
     C*
     C     FLDNAM        WHENEQ    'SCPRC'
     C                   CALL      'W2136R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANK        ##KEY            10
     C*
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C                   SETON                                        90
     C                   Z-ADD     FLDNUM        X
     C*
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         RC(X)                          Payment Status
     C*
     C     X             IFEQ      5
     C                   MOVEA     '0100'        *IN(86)                        Position Cursor
     C                   ELSE
     C     KJPC          ADD       X             Y                 3 0
     C                   ADD       1             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
     C*
     C                   ELSE                                                   ##KEY Blank
     C     KJPC          ADD       X             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
     C                   ENDIF
     C*
     C*----------------------------------------------------------------_
     C*
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
     C*
     C                   MOVE      *OFF          *INKC
     C                   MOVE      *OFF          *INKL
     C*
     C     QQRTC         IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C*
     C     QQRTC         IFEQ      'B'
     C                   SETON                                        90
     C                   MOVEA     '0101'        *IN(86)
     C                   ENDIF
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      QQFEIN        SCPFEI
     C                   MOVE      QQMOD         SCPMOD
     C                   SETON                                        90
     C                   MOVEA     '1010'        *IN(86)
     C                   ENDIF
     C*
     C*----------------------------------------------------------------_
     C*
     C     HFLD          WHENEQ    'SCPFDT'
     C                   SETON                                        90
     C                   MOVEA     '1011'        *IN(86)
     C*
     C*----------------------------------------------------------------_
     C*
     C     HFLD          WHENEQ    'SCPTDT'
     C                   SETON                                        90
     C                   MOVEA     '0011'        *IN(86)
     C*
     C*----------------------------------------------------------------_
     C*
     C     HFLD          WHENEQ    'SCPGRP'
     C                   SETON                                        90
     C                   MOVEA     '1100'        *IN(86)
     C*
     C*----------------------------------------------------------------_
     C*
     C     HFLD          WHENEQ    'SCPCHK'
     C                   SETON                                        90
     C                   MOVEA     '0111'        *IN(86)
     C*
     C*----------------------------------------------------------------_
     C*
     C     HFLD          WHENEQ    'SCPEOB'
     C                   SETON                                        90
     C                   MOVEA     '1000'        *IN(86)
     C*
     C*----------------------------------------------------------------_
     C*
     C     HFLD          WHENEQ    'SCPIRQ'
     C                   CALL      'SC530R'
     C                   PARM                    SCPIRQ
     C                   PARM                    QQRTCD            1
     C                   SETON                                        90
     C*
     C*  NO SEARCH AVAILABLE
     C*
     C                   OTHER
     C                   SETON                                        4990
     C     KJNUM         IFEQ      99
     C                   MOVE      I6(KJPC)      CH4               4
     C                   MOVEA     CH4           *IN(86)
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C********************************************************************
     C*  PGMEND - Routine prior to *INLR *ON
     C********************************************************************
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR
     C********************************************************************
     C*  PGMSTR - Routine to Initialize program variables
     C********************************************************************
     C     PGMSTR        BEGSR
     C*
     C/COPY SINDIC01
     C*
     C*   LOAD THE OUTPUT CONSTANTS FROM THE PARMS
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVEL     CPRDS         SCPRDS
     C*
     C*  Access User Parameters
     C*
     C     UPKY          CHAIN     PUP01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   ENDIF
     C*
     C                   MOVE      @@CO#         XXCO#             3 0
     C                   MOVE      @@FND         XXFND             3 0
     C                   MOVE      @@FYR         XXFYR             3 0
     C                   MOVE      @@CASE        XXCASE            7 0
     C*
     C                   CALL      'WA006R'
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXFYR
     C                   PARM                    XXCASE
     C                   PARM                    SCCS01
     C                   PARM                    SCCS02
     C*
     C*  Determine if WCMC is installed.
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANKS       ##WCIF            1            WC INST
     C                   PARM      *BLANKS       ##MCIF            1            MC INST
     C                   PARM      *BLANKS       ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANKS       ##AR              1            MC AR INST
     C                   PARM      *BLANKS       ##NTWK            1            NETWORK INST
     C                   PARM      *BLANKS       ##UR              1            PRECERT INST
     C                   PARM      *BLANKS       ##MED             1            MEDMGMT INST
     C                   PARM      *BLANKS       ##EDI             1            EDI INST
     C                   PARM      *BLANKS       ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANKS       ##RHAB            1            REHAB INST
     C                   PARM      *BLANKS       ##MSS             1            MS SUPPORT
     C                   PARM      *BLANKS       ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANKS       ##PRVB            1            PROVIDER BILLS
     C*
     C*  If WCMC is not installed or not interfaced with WCMS, do not
     C*    display/allow the type '3' Managed Care search.
     C*
     C                   SETOFF                                       20
     C     ##MCIF        IFNE      'Y'
     C     ##WCIN        ORNE      'Y'
     C                   SETON                                        20
     C                   ENDIF
     C*
     C*  If WCMC is installed and interfaced, verify that a WCMC group
     C*  exists for the current WCMS group.
     C*
     C     *IN20         IFEQ      *OFF
     C*
     C*
     C                   MOVE      XXCO#         #@CO#
     C                   MOVE      XXFND         #@WCF#
     C                   Z-ADD     XXFYR         ##FYR             3 0
     C                   Z-ADD     XXCASE        ##CASE            7 0
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #@CO#             3
     C                   PARM                    #@WCF#            3
     C                   PARM      *BLANKS       #@SUB#            5
     C                   PARM      *BLANKS       #@FND             3
     C*
     C     ##RTCD        IFEQ      'F'
     C                   SETON                                        20
     C                   ELSE
     C                   MOVE      #@SUB#        ##SUB#            5 0
     C                   MOVE      #@FND         ##GRP#            3 0
     C                   ENDIF
     C                   ENDIF
     C*                                                                *********
     C                   MOVE      @@EOMB        SCPEOB
     C     *IN20         IFEQ      *OFF
     C     @@EOMB        ANDNE     *BLANK
     C                   SETON                                        20
     C                   ENDIF                                                  *IN20 = Off
     C*
     C                   ENDSR
     C********************************************************************
     C*  PREDSP - Routine prior to Display Screen
     C********************************************************************
     C     PREDSP        BEGSR
     C*
     C*
     C                   ENDSR
     C********************************************************************
     C*  PRSCTL - Routine to Call Selected Payment Search
     C********************************************************************
     C     PRSCTL        BEGSR
     C*
     C*  Assign Parameters
     C*
     C                   MOVEA     PS            ##SPS                          Payment Status
     C                   MOVE      SCPFDT        ##SFDT                         Service From Date
     C                   MOVE      SCPTDT        ##STDT                         Service To Date
     C                   MOVEA     PC            ##SPC                          Payment Codes
     C                   MOVEA     TT            ##STT                          Transaction Type
     C                   MOVEA     PV1           ##SPV1                         Payment Value 1
     C                   MOVEA     RC            ##SRC                          Reserve Codes
     C                   MOVE      SCPFEI        ##SFEI                         Federal ID #
     C                   MOVE      SCPCHK        ##SCHK                         Check #
     C                   MOVE      SCPEOB        ##SEOB                         EOMB #
     C                   MOVE      SCPGRP        ##SGRP                         Group vs. Non-Group
     C                   MOVE      SCPMOD        ##SMOD                         modifier
     C*
     C*  Select Requested Search
     C*
     C                   MOVEL(P)  'N'           ##EXHS
     C     UPTITL        IFEQ      'CLIENT'
     C                   MOVEL(P)  'Y'           ##EXHS
     C                   ENDIF
     C*
     C                   CALL      'W0576R'      PAYMNT                 99
     C*
     C*  Assign *IN90 to *ON to allow SCR01 Re-Display
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
