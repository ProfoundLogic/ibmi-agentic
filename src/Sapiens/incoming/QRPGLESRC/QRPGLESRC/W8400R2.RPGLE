     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W8400R2 - GENERATE ERM-6 REPORT
     H***************************************************************
     H** PROGRAM DESCRIPTION:                                      **
     H**                                                           **
     H**   This program generates the ERM-6 State Report.          **
     H**                                                           **
     H**   CALLED FROM:                                            **
     H**         W8400C1                                           **
     H**                                                           **
     H**   CALLS TO:                                               **
     H**         NONE                                              **
     H**                                                           **
     H**   CREATED:  10/26/95   C:30   PROJECT: WC460              **
     H**                                                           **
     H***************************************************************
     H** Indicators:                                               **
     H**                                                           **
     H**  22 *OFF - Count # Lines                                  **
     H**  22 *ON  - Print Line                                     **
     H**  33 *OFF - Do not print Claim data                        **
     H**  33 *ON  - Print Claim data                               **
     H**  44 *ON  - indicates print Accident Date.
     H**  44 *OFF - indicates print Number of Claims.
     H**  60      - General File Access                            **
     H**  61      - WXW3L access                                   **
     H**  62      - WXW4L access                                   **
     H**                                                           **
     H***************************************************************
     H** Files:                                                    **
     H**                                                           **
     H**  WDF2L  - CO#/FND/FYR                                     **
     H**  WMEML  - CO#/FND/EMP#/DIV                                **
     H**  WXW1L  - CO#/FND/EMP#/FYR                                **
     H**  WXW2L  - CO#/FND/EMP#/FYR/Period                         **
     H**  WXW3L  - CO#/FND/EMP#/FYR/Period/CLASS                   **
     H**  WXW4L  - CO#/FND/EMP#/FYR/Period/CLASS/NCCI/STATUS       **
     H**                                                           **
     H***************************************************************
     H* REVISIONS:                                                  *
     H*                                                             *
     H* 06/04/99  C:35   PROJECT: SUPPORT WA046 #17400              *
     H*                  USE CARRIER POLICY NUMBER IN PLACE OF FND/ *
     H*                  EMP#/FYR, IF AVAILABLE.                    *
     H*                                                             *
     H* 02/26/99  C:118  PROJECT:  WC577                            *
     H*                  Updated spelling of 'Cancellation'         *
     H*                                                             *
     H* 12/18/98  C:61 PROJECT 81WNO                                *
     H*                Recompiled because of changes to WXW2P       *
     H*                and WXW3P.                                   *
     H*                                                             *
     H* 09/30/98  C:35   PROJECT: SUPPORT WA071 13520               *
     H*                  SUPPRESS PRINT OF CLASS CODE AND EXPOSURE  *
     H*                  IF MULTIPLE CLAIMS ARE LISTED FOR EACH.    *
     H*                                                             *
     H* PLEASE ADD TO TOP                                           *
     H***************************************************************
     FWDF2L     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWXW1L     IF   E           K DISK
     FWXW2L     IF   E           K DISK
     FWXW3L     IF   E           K DISK
     FWXW4L     IF   E           K DISK
      **** BEGIN ADD **** 06/04/99 *********************************************
     FWMRPL     IF   E           K DISK
      **** END   ADD **** 06/04/99 *********************************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*
     D ST              S              2    DIM(54) CTDATA PERRCD(27)            State Codes
     D*
     D*  Arrays for Each Form Column (Top section of Form)
     D*
     D C2              S              6  0 DIM(6)                               Policy Effect DT
     D C3              S              6  0 DIM(6)                               Policy Effect DT
     D C4              S             10  0 DIM(6)                               Premium
     D C5              S              3  0 DIM(7)                               Period #
     D C6              S              6  0 DIM(7)                               Rate Effect DT
     D C7              S              6  0 DIM(7)                               Mod Effect DT
     D C7A             S              5  4 DIM(7)                               Experience Mod
     D*  System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*  Policy Number
      **** BEGIN DEL **** 06/04/99 *********************************************
     D*********** DS
     D*************************************** 1  12 XXPOL
     D*************************************** 1   3 XFND
     D*************************************** 4  12 XEMP#
      **** END   DEL **** 06/04/99 *********************************************
      **** BEGIN ADD **** 06/04/99 *********************************************
     D                 DS
     D  XPOL                   1     15
     D  W1FND                  1      3  0
     D  W1EMP#                 4     12  0
     D  W1FYR                 13     15  0
      **** END   ADD **** 06/04/99 *********************************************
     D*  Constant Lines Per Page
     D*  NOTE:  Current Form uses 15 lines per page
     D LPP             C                   CONST(15)

     C****************************************************************
     C*                    K E Y     L I S T S
     C****************************************************************
     C*
     C*   Key List for the Fund Aux file
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   Key List for the Employer Master  file
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*   Key List for Work File # 1
     C*
     C     W1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*   Key List for Work File # 2
     C*
     C     W2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*   Key List for Work File # 3
     C*
     C     W3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXPER#
     C                   KFLD                    XXCLAS
     C*
     C     W3KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*   Key List for Work File # 4
     C*
     C     W4KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXPER#
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXNCCI
     C                   KFLD                    XXSTAT
     C*
     C     W4KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
      **** BEGIN ADD **** 06/04/99 ************************************
      *
      * KEY LIST FOR THE CARRIER/STATE REPORTING FILE
      *
     C     RPKY          KLIST
     C                   KFLD                    W1CO#
     C                   KFLD                    W1FND
     C                   KFLD                    W1EMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    W1FYR
      **** END   ADD **** 06/04/99 ************************************
     C*
     C     *LIKE         DEFINE    W1CO#         XXCO#
     C     *LIKE         DEFINE    W1FND         XXFND
     C     *LIKE         DEFINE    W1FYR         XXFYR
     C     *LIKE         DEFINE    W1EMP#        XXEMP#
     C     *LIKE         DEFINE    W2PER#        XXPER#
     C     *LIKE         DEFINE    W4CLAS        XXCLAS
     C     *LIKE         DEFINE    W4NCCI        XXNCCI
     C     *LIKE         DEFINE    W4STAT        XXSTAT
     C     *LIKE         DEFINE    EMDT          XXDATE
      **** BEGIN ADD **** 09/30/98 *********************************************
     C     *LIKE         DEFINE    W3PER#        XXHPER
     C     *LIKE         DEFINE    W3CLAS        XXHCLS
      **** END   ADD **** 09/30/98 *********************************************
      **** BEGIN ADD **** 06/04/99 *********************************************
     C                   MOVE      *BLANKS       XXPOL            18
      **** END   ADD **** 06/04/99 *********************************************
     C*
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQEFDT            8
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
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*================================================================
     C*
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   MOVE      QQEFDT        X1EFDT            8 0
     C*
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXEFDT            6 0
     C*
     C*   Workfile # 1 will be positioned to read the Next Employer.
     C*   If the first READ results in Indicator 60 *ON, End Program.
     C*
     C                   READ      PW101                                  60
     C*
     C     *IN60         DOWEQ     *OFF                                         First W1 READ
     C*
     C*  Assign Key Fields
     C*
     C                   Z-ADD     W1CO#         XXCO#
     C                   Z-ADD     W1FND         XXFND
     C                   Z-ADD     W1EMP#        XXEMP#
     C*
     C*  Initialize Arrays
     C*
     C                   MOVEA     *ZERO         C2
     C                   MOVEA     *ZERO         C3
     C                   MOVEA     *ZERO         C4
     C                   MOVEA     *ZERO         C5
     C                   MOVEA     *ZERO         C6
     C                   MOVEA     *ZERO         C7
     C                   MOVEA     *ZERO         C7A
     C*
     C*     P r o c e s s     W o r k f i l e     # 1
     C*
     C                   Z-ADD     *ZERO         I                 3 0
     C*
     C     *IN60         DOWEQ     *OFF                                         W1 READE
     C*
     C*  Assign Column Arrays 2, 3, 4
     C*
     C                   ADD       1             I
     C                   Z-ADD     W1PFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         C2(I)
     C*
     C                   Z-ADD     W1PTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         C3(I)
     C*
     C                   Z-ADD     W1PREM        C4(I)                          Premium
     C*
     C     W1KY          READE     PW101                                  60
     C                   ENDDO
     C*
     C                   XFOOT     C4            TOTPRM           10 0
     C*
     C*     P r o c e s s     W o r k f i l e     # 2
     C*
     C                   Z-ADD     *ZERO         I                 3 0
     C     W2KY          SETLL     PW201
     C     W2KY          READE     PW201                                  60
     C*
     C     *IN60         DOWEQ     *OFF                                         W2 READE
     C*
     C*  Assign Column Arrays 5, 6, 7, 7A
     C*
     C                   ADD       1             I
     C                   Z-ADD     W2PER#        C5(I)                          Period #
     C*
     C                   Z-ADD     W2RTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         C6(I)
     C*
     C                   Z-ADD     W2MDDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         C7(I)
     C*
     C                   Z-ADD     W2EXMD        C7A(I)                         Exp. Mod.
     C*
     C     W2KY          READE     PW201                                  60
     C                   ENDDO
     C*
     C*     P r o c e s s     T o p     o f     F o r m     F i e l d s
     C*
     C*  Get the Fund Aux file
     C*
     C     F2KY          CHAIN     PF201                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     *ZERO         F2CAR#
     C                   ENDIF
     C*
     C*  Get the Employer Master
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANK        EMNAME
     C                   ENDIF
      **** BEGIN ADD **** 06/04/99 *********************************************
      *
      * GET THE STATE REPORTING DIVISION.  IF NO STATE REPORTING DIVISION,
      * DEFAULT TO THE FIRST DIVISION.
      *
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             EMDIV
     C                   ELSE
      *
     C     *IN60         DOWEQ     *OFF
     C     EMSTRP        ANDNE     'Y'
     C     EMKY          READE     PEM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C     *IN60         IFEQ      *ON
     C     EMKY          CHAIN     PEM01                              60
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
      **** END   ADD **** 06/04/99 *********************************************
     C*
     C*  Assign State code
     C*
     C     EMPST         IFNE      *BLANK
     C     EMPST         LOOKUP    ST(I)                                  60
     C                   ELSE
     C     EMMST         LOOKUP    ST(I)                                  60
     C                   ENDIF
     C*
     C                   Z-ADD     I             XXMST             2 0
     C*
     C*  Assign Policy Number
     C*
      **** BEGIN CHG **** 06/04/99 *********************************************
     C******************** MOVE XXFND     XFND
     C******************** MOVE XXEMP#    XEMP#
     C                   MOVEL(P)  XPOL          XXPOL
      *
      * CHECK FOR A CARRIER/STATE REPORTING RECORD
      *
     C     RPKY          CHAIN     PRP01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     RPCAP#        IFNE      *BLANKS
      *
     C                   MOVEL(P)  RPCAP#        XXPOL
      *
     C                   ENDIF                                                  RPCAP# IFNE *BLANKS
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
      **** END   CHG **** 06/04/99 *********************************************
     C*
     C*     C a l c u l a t e     #     o f     P a g e s
     C*
     C                   MOVE      *OFF          *IN22
     C                   EXSR      SR#PG
     C*
     C*     P r i n t     T o p     S e c t i o n     o f    F o r m
     C*
     C                   Z-ADD     1             PG#               3 0
     C                   EXSR      SRTOP
     C*
     C*     P r o c e s s     W o r k f i l e s    # 3    &    # 4
     C*
     C                   Z-ADD     *ZERO         I
     C                   MOVE      *ON           *IN22
     C                   EXSR      SR#PG
     C*
     C*     P o s i t i o n     W o r k f i l e     # 1
     C*
     C     W1KY          SETGT     PW101
     C                   READ      PW101                                  60
     C*
     C                   ENDDO                                                  *IN60 DOWEQ 0
     C*
     C*  End Program
     C*
     C                   MOVE      *ON           *INLR
     C*
     C**********************************************************************
     C*  SR#PG - Subroutine used for 2 purposes:
     C*          1) *IN22 *OFF - Count Lines to Determine # Pages
     C*          2) *IN22 *ON  - Print Exposure / Claim lines
     C**********************************************************************
     C     SR#PG         BEGSR
     C*
     C     *IN22         IFEQ      *ON
     C                   EXCEPT    HEAD2
     C                   ENDIF
     C*
     C                   Z-ADD     *ZERO         I                 3 0
      **** BEGIN ADD **** 09/30/98 *********************************************
     C                   Z-ADD     0             XXHPER
     C                   Z-ADD     0             XXHCLS
      **** END   ADD **** 09/30/98 *********************************************
     C*
     C*  Position Workfile #3 and #4 at the first Employer record
     C*
     C     W3KY1         SETLL     PW301
     C     W3KY1         READE     PW301                                  61
     C*
     C     W4KY1         SETLL     PW401
     C     W4KY1         READE     PW401                                  62
     C*
     C*  Process both workfiles until all Employer records accessed.
     C*
     C     *IN61         DOWEQ     *OFF
     C     *IN62         OREQ      *OFF
     C*
     C                   SELECT
     C*  Case (1)
     C     *IN61         WHENEQ    *ON
     C*
     C     *IN62         DOWEQ     *OFF
     C                   MOVE      *ON           *IN33
     C                   EXSR      SRPROC
     C     W4KY1         READE     PW401                                  62
     C                   ENDDO
     C*  Case (2)
     C     *IN62         WHENEQ    *ON
     C*
     C     *IN61         DOWEQ     *OFF
     C                   MOVE      *OFF          *IN33
     C                   EXSR      SRPROC
     C     W3KY1         READE     PW301                                  61
     C                   ENDDO
     C*  Case (3)
     C     W3CLAS        WHENEQ    W4CLAS
     C     W3PER#        ANDEQ     W4PER#
     C*
     C     W3CLAS        DOWEQ     W4CLAS
     C     W3PER#        ANDEQ     W4PER#
     C     *IN62         ANDEQ     *OFF
     C                   MOVE      *ON           *IN33
     C                   EXSR      SRPROC
     C     W4KY1         READE     PW401                                  62
     C                   ENDDO
     C     W3KY1         READE     PW301                                  61
     C*  Case (4)
     C     W3CLAS        WHENLT    W4CLAS
     C     W3PER#        ANDEQ     W4PER#
     C*
     C     W3CLAS        DOWLT     W4CLAS
     C     W3PER#        ANDEQ     W4PER#
     C     *IN61         ANDEQ     *OFF
     C                   MOVE      *OFF          *IN33
     C                   EXSR      SRPROC
     C     W3KY1         READE     PW301                                  61
     C                   ENDDO
     C*  Case (5)
     C     W3CLAS        WHENGT    W4CLAS
     C     W3PER#        ANDEQ     W4PER#
     C*
     C     W3CLAS        DOWGT     W4CLAS
     C     W3PER#        ANDEQ     W4PER#
     C     *IN62         ANDEQ     *OFF
     C*  Skip Claims that do not have valid Payroll Class assigned
     C     W4KY1         READE     PW401                                  62
     C                   ENDDO
     C*  Case (6)
     C     W3PER#        WHENLT    W4PER#
     C*
     C     W3PER#        DOWNE     W4PER#
     C     *IN61         ANDEQ     *OFF
     C                   MOVE      *OFF          *IN33
     C                   EXSR      SRPROC
     C     W3KY1         READE     PW301                                  61
     C                   ENDDO
     C*  Case (7)
     C     W3PER#        WHENGT    W4PER#
     C*
     C     W3PER#        DOWNE     W4PER#
     C     *IN62         ANDEQ     *OFF
     C                   MOVE      *ON           *IN33
     C                   EXSR      SRPROC
     C     W4KY1         READE     PW401                                  62
     C                   ENDDO
     C*
     C                   ENDSL
     C*
     C                   ENDDO
     C*
     C*  Calculate Number of Pages using constant LPP if executing
     C*  subroutine to count lines.
     C*
     C     *IN22         IFEQ      *OFF
     C     I             DIV       LPP           PGTOT             3 0
     C                   MVR                     REM               3 0
     C     REM           IFGT      *ZERO
     C                   ADD       1             PGTOT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C**********************************************************************
     C*  SRTOP - Subroutine to Print the Top Section of Form
     C**********************************************************************
     C     SRTOP         BEGSR
     C*
     C                   EXCEPT    HEAD
     C*
     C                   DO        6             I
     C                   EXCEPT    TOP
     C                   ENDDO
     C*
     C                   EXCEPT    TOPTOT
     C*
     C                   ENDSR
     C*
     C**********************************************************************
     C*  SRPROC - Subroutine used with subroutine SR#PG for 2 purposes:
     C*          1) *IN22 *OFF - Count Lines to Determine # Pages
     C*          2) *IN22 *ON  - Print Exposure / Claim lines
     C*
     C*  If *IN22 *ON,
     C*  1) *IN33 *ON  indicates print Claim Data.
     C*     *IN33 *OFF indicates do NOT print Claim Data.
     C*  If *IN33 *ON,
     C*  2) *IN44 *ON  indicates print Accident Date.
     C*     *IN44 *OFF indicates print Number of Claims.
     C**********************************************************************
     C     SRPROC        BEGSR
     C*
     C*  Count the record and exit the subroutine.
     C*
     C     *IN22         IFEQ      *OFF
     C                   ADD       1             I
     C                   GOTO      SREND
     C                   ENDIF
      **** BEGIN ADD **** 09/30/98 *********************************************
     C     W3PER#        IFNE      XXHPER
     C     W3CLAS        ORNE      XXHCLS
     C                   MOVE      W3PER#        XXHPER
     C                   MOVE      W3CLAS        XXHCLS
     C                   SETON                                        35  36
     C                   ELSE
     C                   SETOFF                                       35  36
     C                   ENDIF                                                  W3PER# IFNE XXHPER
      **** END   ADD **** 09/30/98 *********************************************
     C*
     C*          P r i n t     R o u t i n e
     C*
     C*  Check for End of Page condition
     C*
     C     I             IFEQ      LPP
     C                   ADD       1             PG#
     C                   EXSR      SRTOP
     C                   EXCEPT    HEAD2
     C                   Z-ADD     *ZERO         I
      **** BEGIN ADD **** 09/30/98 *********************************************
     C                   SETON                                        36
      **** END   ADD **** 09/30/98 *********************************************
     C                   ENDIF
     C*
     C*  If valid Accident Date, reformat the date.
     C*
     C     *IN33         IFEQ      *ON
     C     W4ACDT        ANDGT     *ZERO
     C                   MOVE      *ON           *IN44
     C*
     C                   Z-ADD     W4ACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X4ACDT            6 0
     C*
     C                   ELSE
     C                   MOVE      *OFF          *IN44
     C                   ENDIF
     C*
     C*  Print Detail Line
     C*
     C                   EXCEPT    BOT
     C                   ADD       1             I
     C*
     C     SREND         TAG
     C*
     C                   ENDSR
     C**********************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                                           55 'WORKERS COMPENSATION STA'
     O                                           73 'TISTICAL REPORTING'
     O          E            HEAD           3
     O                                           65 'FORM FOR EXPERIENCE RATI'
     O                                           67 'NG'
     O                                           90 'Page'
     O                       PG#           4     93
     O                                           96 ' of'
     O                       PGTOT         4     99
     O          E            HEAD           2
     O                                           15 'Policy Number'
     O                                           40 'State'
     O                                           70 'Data Provider Number'
     O                                          100 'Rating Effective Date'
     O          E            HEAD           2
      **** BEGIN CHG **** 06/04/99 *********************************************
     O************************ XXPOL     14
     O                       XXPOL               20
      **** END   CHG **** 06/04/99 *********************************************
     O                       XXMST               43
     O                       F2CAR#        4     64
     O                       XXEFDT        Y     94
     O          E            HEAD           2
     O                                           14 'Insured Name'
     O          E            HEAD           2
     O                       EMNAME              42
     O          E            HEAD           1
     O                                           80 'Split Rate / Mod'
     O          E            HEAD           1
     O                                           10 'Period'
     O                                           27 'Policy'
     O                                           47 'Policy Expiration'
     O                                           63 'Total Subject'
     O                                           73 'Period'
     O                                           85 'Rate'
     O                                           97 'Mod'
     O                                          104 'Exp'
     O          E            HEAD           1
     O                                           27 'Effective Date'
     O                                           47 'or'
     O                                           63 'Premium per'
     O                                           85 'Effective'
     O                                           97 'Effective'
     O                                          104 'Mod'
     O          E            HEAD           2
     O***** BEGIN CHANGE * 2/26/99 ************************************
     O*********************************  47 'Cancelation Date'*********
     O                                           47 'Cancellation Date'
     O***** END CHANGE *** 2/26/99 ************************************
     O                                           63 'Policy Period'
     O                                           85 'Date'
     O                                           97 'Date'
     O          E            TOP            1
     O                       I             4     10
     O                       C2(I)               27 '  /  /  '
     O                       C3(I)               47 '  /  /  '
     O                       C4(I)         4     63
     O                       C5(I)         4     73
     O                       C6(I)               85 '  /  /  '
     O                       C7(I)               97 '  /  /  '
     O                       C7A(I)        4    104
     O          E            TOPTOT         2
     O                                           50 'Total Subject Premium'
     O                       TOTPRM        4     63
     O          E            HEAD2       2  1
     O                                           10 'Period'
     O                                           17 'Class'
     O                                           29 'Exposure'
     O                                           43 'Claim'
     O                                           55 'Accident'
     O                                           59 'I'
     O                                           70 'Indemnity'
     O                                           81 'Medical'
     O                                           87 'Loss'
     O                                           95 'Claim'
     O                                          100 'Cat'
     O          E            HEAD2          1
     O                                           17 'Code'
     O                                           43 'Number'
     O                                           55 'Date/No of'
     O                                           59 'N'
     O                                           87 'Cov'
     O                                           95 'Status'
     O                                          100 'No'
     O          E            HEAD2          2
     O                                           55 'Claims'
     O                                           59 'J'
     O          E            BOT            1
     O                       W3PER#        4     10
      **** BEGIN CHG **** 09/30/98 *********************************************
     O************************ W3CLAS4   17
     O************************ W3EAMT4   29
     O               36      W3CLAS        4     17
     O               35      W3EAMT        4     29
      **** END   CHG **** 09/30/98 *********************************************
     O               33      W4CLM               43
     O               33 44   X4ACDT              55 '  /  /  '
     O               33N44   W4NCLM        4     55
     O               33      W4NCCI              59
     O               33      W4COMP        4     70
     O               33      W4MED         4     81
     O               33      W4LCC         4     87
     O               33      W4STAT              95
**  TABLE ST - STATES IN ENDORSEMENT NUMERICAL ORDER
ALAZARCACOCTDEDCFLGAIDILINIAKSKYLAMEMDMAMIMNMSMOMTNENV
NHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIWYPRZZHIZZAK
