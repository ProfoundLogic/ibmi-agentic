     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR  dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE U7060R - PROCEDURE/DIAGNOSIS ANALYSIS REPORT - PRT PGM
     H*****************************************************************
     H*  INDICATORS:                                                  *
     H*  08    Batch Mode                                             *
     H*  09    Demand Mode                                            *
     H*  60    General Chain/Usage                                    *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*  08/05/13 C:277 PROJECT: MC329 Phase II                       *
     H*                 Print ICD10 is ICD10 is active else Print ICD9*
     H*                                                               *
     H* 01/23/13 C:287 Project: SY266_QRY                             *
     H*                Add DFTACTGRP and ACTGRP compile options       *
     H* 01/28/10 C:249 Project: SY215                                 *
     H*          C:189 Retrieve brand Name                            *
     H*                                                               *
     H*  03/14/00 C:62 PROJECT: MCMNT                                 *
     H*                CHANGE DATE FORMATS TO 8.0                     *
     H*                                                               *
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMEML     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     F                                     RENAME(PPD01:PPD02)
     FUXPDL     IF   E           K DISK    USROPN
     FUXPDL1    IF   E           K DISK    USROPN
     F                                     RENAME(PPD01:PPD03)
     FUXPDL2    IF   E           K DISK    USROPN
     F                                     RENAME(PPD01:PPD04)
     FUXPDL3    IF   E           K DISK    USROPN
     F                                     RENAME(PPD01:PPD05)
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOF)
     F*

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  ATYP                   1      5
     D                                     DIM(5)                               ADMISSION TYPE
     D  XXATP1                 1      1
     D  XXATP2                 2      2
     D  XXATP3                 3      3
     D  XXATP4                 4      4
     D  XXATP5                 5      5
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** Beg Add *** 08/05/13 ***************************************
     D xxICD           s              7
     D*** End Add *** 08/05/13 ***************************************

     C*
     C* COMPNAY MASTER FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C* OUTPUT ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C*   KEY LIST FOR THE SUBSCRIBER
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C* USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* PROVIDER FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQSEL             1
     C                   PARM                    QQATP1            1
     C                   PARM                    QQATP2            1
     C                   PARM                    QQATP3            1
     C                   PARM                    QQATP4            1
     C                   PARM                    QQATP5            1
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    PDCO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    PDSUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    PDFND         XXFND                          3.0N
     C     *LIKE         DEFINE    SMNAME        XXSBNM                         40A
     C     *LIKE         DEFINE    FNNAME        XXFNNM                         60A
     C     *LIKE         DEFINE    EMNAME        XXEMNM                         40A
     C     *LIKE         DEFINE    EMEMP#        HLEMP#                         5.0N
     C     *LIKE         DEFINE    ORTITL        XXTITL                         40A
     C     *LIKE         DEFINE    PDFEIN        XXFEIN                         9.0N
     C     *LIKE         DEFINE    PDMOD         XXMOD                          3.0N
     C     *LIKE         DEFINE    PDNAME        XXPDNM                         40A
     C     *LIKE         DEFINE    EMEMP#        XXEMP#                         9.0N
     C*
     C                   Z-ADD     *ZEROS        XXFRDT            6 0          6.0N
     C                   Z-ADD     *ZEROS        XXTODT            6 0          6.0N
     C                   MOVE      *BLANKS       XXSEL             1            1A
     C                   MOVE      *BLANKS       XXTYPE            1            1A
     C                   MOVE      *BLANKS       XXATP1            1            1A
     C                   MOVE      *BLANKS       XXATP2            1            1A
     C                   MOVE      *BLANKS       XXATP3            1            1A
     C                   MOVE      *BLANKS       XXATP4            1            1A
     C                   MOVE      *BLANKS       XXATP5            1            1A
     C                   MOVE      *BLANKS       XXPRT             1            1A
     C                   MOVE      'Y'           XFIRST            1            1A
     C                   Z-ADD     *ZEROS        X                 5 0          5.0N
     C* EMPLOYER TOTAL WORK FIELDS
     C                   Z-ADD     *ZEROS        ETTACT            5 0          TOTAL ACT
     C                   Z-ADD     *ZEROS        ETAPPR            5 0          TOTAL APPR
     C                   Z-ADD     *ZEROS        ETREQD            5 0          TOTAL REQD
     C                   Z-ADD     *ZEROS        ETRECD            5 0          TOTAL RECD
     C                   Z-ADD     *ZEROS        ETASAV            5 0          TOTAL DYS APPR SAV
     C                   Z-ADD     *ZEROS        ETDENY            5 0          TOTAL DENY
     C                   Z-ADD     *ZEROS        ETDSAV            5 0          TOTAL DYS DENY SAV
     C                   Z-ADD     *ZEROS        ETTSAV            5 0          TOTAL SAV
     C* SUMMARY REPORT WORK FIELDS
     C                   Z-ADD     *ZEROS        XXPRPT            5 4          % WORKFIELD
     C                   Z-ADD     *ZEROS        APPRPT            5 2          % APPR
     C                   Z-ADD     *ZEROS        ASAVPT            5 2          % APPR SAV
     C                   Z-ADD     *ZEROS        DENTPT            5 2          % DENY
     C                   Z-ADD     *ZEROS        DSAVPT            5 2          % DENY SAV
     C                   Z-ADD     *ZEROS        TSAVPT            5 2          % TOTAL SAV
     C* SUMMARY REPORT TOTAL WORKFIELDS
     C                   Z-ADD     *ZEROS        TTTACT            5 0          TOTAL ACT
     C                   Z-ADD     *ZEROS        TTAPPR            5 0          TOTAL APPR
     C                   Z-ADD     *ZEROS        TTAPPT            5 2          TOTAL % APPR
     C                   Z-ADD     *ZEROS        TTASPT            5 2          TOTAL % APPR SAV
     C                   Z-ADD     *ZEROS        TTREQD            5 0          TOTAL REQD
     C                   Z-ADD     *ZEROS        TTRECD            5 0          TOTAL RECD
     C                   Z-ADD     *ZEROS        TTASAV            5 0          TOTAL APPR SAV DAYS
     C                   Z-ADD     *ZEROS        TTAREC            5 0          TOTAL REC DAYS
     C                   Z-ADD     *ZEROS        TTDENY            5 0          TOTAL DENY
     C                   Z-ADD     *ZEROS        TTDEPT            5 2          TOTAL % DENY
     C                   Z-ADD     *ZEROS        TTDSAV            5 0          TOTAL DENY SAV DAYS
     C                   Z-ADD     *ZEROS        TTDSPT            5 2          TOTAL % DENY SAV
     C                   Z-ADD     *ZEROS        TTTSAV            5 0          TOTAL TOTAL SAV
     C                   Z-ADD     *ZEROS        TTSVPT            5 2          TOTAL % TOTAL SAV
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQFEIN        XXFEIN
     C                   MOVE      QQMOD         XXMOD
     C                   MOVE      QQFRDT        X1FRDT            8 0
     C*
     C                   Z-ADD     X1FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   MOVE      QQTODT        X1TODT            8 0
     C*
     C                   Z-ADD     X1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   MOVE      QQSEL         XXSEL
     C                   MOVE      QQATP1        XXATP1
     C                   MOVE      QQATP2        XXATP2
     C                   MOVE      QQATP3        XXATP3
     C                   MOVE      QQATP4        XXATP4
     C                   MOVE      QQATP5        XXATP5
     C                   MOVE      QQTYPE        XXTYPE
     C*
     C                   TIME                    DSDTTM
     C*
     C* SET-UP AND INITIALIZE
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   SET INDICATORS TO DETERMINE WHICH REPORTS TO PRINT
     C*   SUMMARY/DETAIL
     C*
     C     XXTYPE        IFEQ      'B'
     C                   MOVE      '1'           *IN15
     C                   MOVE      '1'           *IN16
     C                   ELSE
     C     XXTYPE        IFEQ      'D'
     C                   MOVE      '1'           *IN15
     C                   ELSE
     C     XXTYPE        IFEQ      'S'
     C                   MOVE      '1'           *IN16
     C                   END
     C                   END
     C                   END
     C*
     C* CHECK THE SORT SELECTION
     C*
     C                   MOVE      '0'           *IN18
     C                   MOVE      '0'           *IN19
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN21
     C                   MOVE      '0'           *IN22
     C                   MOVE      '0'           *IN23
     C                   MOVE      '0'           *IN24
     C                   MOVE      '0'           *IN25
     C                   MOVE      '0'           *IN26
     C                   MOVE      '0'           *IN27
     C*
     C     XXEMP#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN18
     C                   END
     C*
     C     XXFEIN        IFNE      *ZEROS
     C                   MOVE      '1'           *IN19
     C                   END
     C*
     C     XXSEL         IFEQ      'P'
     C                   MOVE      '1'           *IN20
     C                   ELSE
     C                   MOVE      '0'           *IN20
     C                   END
     C*
     C     XXSUB#        IFNE      *ZEROS
     C                   MOVE      '1'           *IN21
     C                   END
     C*
     C* CHECK FOR ADMISSION TYPE SELECTION
     C*
     C                   Z-ADD     1             X
     C     'M'           LOOKUP    ATYP(X)                                70
     C     *IN70         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   END
     C*
     C                   Z-ADD     1             X
     C     'S'           LOOKUP    ATYP(X)                                70
     C     *IN70         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   END
     C*
     C                   Z-ADD     1             X
     C     'Y'           LOOKUP    ATYP(X)                                70
     C     *IN70         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   END
     C*
     C                   Z-ADD     1             X
     C     'P'           LOOKUP    ATYP(X)                                70
     C     *IN70         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   END
     C*
     C                   Z-ADD     1             X
     C     'C'           LOOKUP    ATYP(X)                                70
     C     *IN70         IFEQ      '1'
     C                   MOVE      '1'           *IN26
     C                   END
     C*
     C     *IN22         IFEQ      '0'
     C     *IN23         ANDEQ     '0'
     C     *IN24         ANDEQ     '0'
     C     *IN25         ANDEQ     '0'
     C     *IN26         ANDEQ     '0'
     C                   MOVE      '1'           *IN27
     C                   END
     C*
     C* GET THE USER PARAMETERS FILE.
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C   60              MOVE      *BLANK        UPUSNM
     C*
     C* CHAIN TO THE COMPANY FILE
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*
     C* GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C* PRINT THE DISTRIBUTION INFORMATION
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVEL     ORTITL        XXTITL
      ***** Beg Del ***** 08/26/14 **************************************
     C********   15              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 **************************************
     C*
     C                   END
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR QSYSPRT1
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 **************************************
     C********   16              EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 **************************************
     C*
     C                   END
     C*
     C* GET THE SUBSCRIBER INFORMATION FOR THE HEADING
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   END
     C*
     C* GET THE FUND INFORMATION FOR THE HEADING
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C*
     C* GET THE EMP# INFORMATION FOR THE HEADING
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        XXEMNM
     C                   Z-ADD     XXEMP#        HLEMP#
     C                   ELSE
     C                   MOVE      *BLANKS       XXEMNM
     C                   Z-ADD     *HIVAL        HLEMP#
     C                   END
     C*
     C* GET THE PROV INFORMATION FOR THE HEADING
     C*
     C     PDKEY         CHAIN     PPD02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PDNAME        XXPDNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXPDNM
     C                   END
     C*
     C* PRINT HEADINGS
     C*
     C   15
     CAN 21
     CANN18              EXCEPT    HEAD
     C   16
     CAN 21
     CANN18              EXCEPT    HEAD1
     C   15
     CAN 19              EXCEPT    HEAD
     C   16
     CAN 19              EXCEPT    HEAD1
     C*
     C* PROCESS THE CORRECT LOGICAL
     C*
     C  N19
     CANN20              OPEN      UXPDL
     C  N19
     CAN 20              OPEN      UXPDL1
     C   19
     CANN20              OPEN      UXPDL2
     C   19
     CAN 20              OPEN      UXPDL3
     C*
     C  N19
     CANN20              READ      PPD01                                  50
     C  N19
     CAN 20              READ      PPD03                                  50
     C   19
     CANN20              READ      PPD04                                  50
     C   19
     CAN 20              READ      PPD05                                  50
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If PDICDIND = 'Y';
          xxICd = PDicd10;
       Else;
          xxICd = PDicd9;
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C* IF NO RECORDS PRINT ERROR MESSAGE
     C*
     C     *IN50         IFEQ      '1'
     C   15              EXCEPT    ERRMSG
     C   16              EXCEPT    ERRMS1
     C                   GOTO      TERM
     C                   END                                                    END *IN50
     C*
     C* START LOOP
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C* CHECK FOR CHANGE IN EMPLOYER IF CO/SUB/FND/EMP SELECTED
     C*
     C     *IN21         IFEQ      '1'
     C     *IN18         ANDEQ     '1'
     C     PDEMP#        IFNE      HLEMP#
     C                   EXSR      SREMP
     C                   END
     C                   END
     C*
     C* IF DETAIL REPORT IS SELECTED EXCEPT DETAIL LINE
     C*
     C     *INOA         IFEQ      '1'
     C   15              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C   15              EXCEPT    DET
     C*
     C* ACCUMULATE THE EMPLOYER TOTALS
     C*
     C                   ADD       PDTACT        ETTACT
     C                   ADD       PDAPPR        ETAPPR
     C                   ADD       PDREQD        ETREQD
     C                   ADD       PDRECD        ETRECD
     C                   ADD       PDASAV        ETASAV
     C                   ADD       PDDENY        ETDENY
     C                   ADD       PDDSAV        ETDSAV
     C                   ADD       PDTSAV        ETTSAV
     C*
     C     *IN16         IFEQ      '1'
     C*
     C                   Z-ADD     *ZEROS        APPRPT
     C                   Z-ADD     *ZEROS        ASAVPT
     C                   Z-ADD     *ZEROS        DENTPT
     C                   Z-ADD     *ZEROS        DSAVPT
     C                   Z-ADD     *ZEROS        TSAVPT
     C                   Z-ADD     *ZEROS        HLRECD            5 0
     C*
     C     PDTACT        IFNE      *ZEROS
     C     PDAPPR        DIV       PDTACT        XXPRPT
     C     XXPRPT        MULT(H)   100           APPRPT
     C                   END
     C*
     C     PDTSAV        IFNE      *ZEROS
     C     PDASAV        DIV       PDTSAV        XXPRPT
     C     XXPRPT        MULT(H)   100           ASAVPT
     C                   END
     C*
     C     PDTACT        IFNE      *ZEROS
     C     PDDENY        DIV       PDTACT        XXPRPT
     C     XXPRPT        MULT(H)   100           DENTPT
     C                   END
     C*
     C     PDTSAV        IFNE      *ZEROS
     C     PDDSAV        DIV       PDTSAV        XXPRPT
     C     XXPRPT        MULT(H)   100           DSAVPT
     C                   Z-ADD     PDDSAV        HLRECD
     C                   END
     C*
     C     HLRECD        ADD       PDRECD        HLRECD
     C     HLRECD        IFNE      *ZEROS
     C     PDTSAV        DIV       HLRECD        XXPRPT
     C     XXPRPT        MULT(H)   100           TSAVPT
     C                   END
     C*
     C                   ADD       PDTACT        TTTACT
     C                   ADD       PDAPPR        TTAPPR
     C                   ADD       PDREQD        TTREQD
     C                   ADD       PDRECD        TTRECD
     C                   ADD       HLRECD        TTAREC
     C                   ADD       PDASAV        TTASAV
     C                   ADD       PDDENY        TTDENY
     C                   ADD       PDDSAV        TTDSAV
     C                   ADD       PDTSAV        TTTSAV
     C*
     C     TTTACT        IFNE      *ZEROS
     C     TTAPPR        DIV       TTTACT        XXPRPT
     C     XXPRPT        MULT(H)   100           TTAPPT
     C                   ELSE
     C                   Z-ADD     *ZEROS        TTAPPT
     C                   END
     C*
     C     TTTSAV        IFNE      *ZEROS
     C     TTASAV        DIV       TTTSAV        XXPRPT
     C     XXPRPT        MULT(H)   100           TTASPT
     C                   ELSE
     C                   Z-ADD     *ZEROS        TTASPT
     C                   END
     C*
     C     TTTACT        IFNE      *ZEROS
     C     TTDENY        DIV       TTTACT        XXPRPT
     C     XXPRPT        MULT(H)   100           TTDEPT
     C                   ELSE
     C                   Z-ADD     *ZEROS        TTDEPT
     C                   END
     C*
     C     TTTSAV        IFNE      *ZEROS
     C     TTDSAV        DIV       TTTSAV        XXPRPT
     C     XXPRPT        MULT(H)   100           TTDSPT
     C                   ELSE
     C                   Z-ADD     *ZEROS        TTDSPT
     C                   END
     C*
     C     TTAREC        IFNE      *ZEROS
     C     TTTSAV        DIV       TTAREC        XXPRPT
     C     XXPRPT        MULT(H)   100           TTSVPT
     C                   ELSE
     C                   Z-ADD     *ZEROS        TTSVPT
     C                   END
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    DET1
     C                   END
     C*
     C  N19
     CANN20              READ      PPD01                                  50
     C  N19
     CAN 20              READ      PPD03                                  50
     C   19
     CANN20              READ      PPD04                                  50
     C   19
     CAN 20              READ      PPD05                                  50
     C*
     C                   END                                                    END DOW 50
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If PDICDIND = 'Y';
          xxICd = PDicd10;
       Else;
          xxICd = PDicd9;
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C* PRINT TOTALS FOR PROCESSED EMPLOYER
     C*
     C     *INOA         IFEQ      '1'
     C   15              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C   15              EXCEPT    TTEMP
     C     *INOF         IFEQ      '1'
     C   16              EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
     C                   END
     C   16              EXCEPT    TTRPT
     C*
     C* END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found and (xxtype = 'B' or xxtype = 'D');
          Except Route;
       Endif;
       xxprt = '1';
       Chain Orkey Por01;
       If %found and (xxtype = 'B' or xxtype = 'S');
          Except Route1;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*****************************************************************
     C* SREMP - SUBROUTINE TO GET NEW EMPLOYER NAME
     C*****************************************************************
     C     SREMP         BEGSR
     C*
     C     XFIRST        IFEQ      ' '
     C*
     C* PRINT THE TOTAL LINE
     C*
     C     *INOA         IFEQ      '1'
     C   15              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C   15              EXCEPT    TTEMP
     C     *INOF         IFEQ      '1'
     C   16              EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
     C                   END
     C   16              EXCEPT    TTRPT
     C*
     C* ZERO OUT TOTAL WORK FILEDS
     C*
     C                   Z-ADD     *ZEROS        ETTACT
     C                   Z-ADD     *ZEROS        ETAPPR
     C                   Z-ADD     *ZEROS        ETREQD
     C                   Z-ADD     *ZEROS        ETRECD
     C                   Z-ADD     *ZEROS        ETASAV
     C                   Z-ADD     *ZEROS        ETDENY
     C                   Z-ADD     *ZEROS        ETDSAV
     C                   Z-ADD     *ZEROS        ETTSAV
     C*
     C                   Z-ADD     *ZEROS        TTTACT
     C                   Z-ADD     *ZEROS        TTAPPR
     C                   Z-ADD     *ZEROS        TTAPPT
     C                   Z-ADD     *ZEROS        TTASPT
     C                   Z-ADD     *ZEROS        TTREQD
     C                   Z-ADD     *ZEROS        TTRECD
     C                   Z-ADD     *ZEROS        TTASAV
     C                   Z-ADD     *ZEROS        TTAREC
     C                   Z-ADD     *ZEROS        TTDENY
     C                   Z-ADD     *ZEROS        TTDEPT
     C                   Z-ADD     *ZEROS        TTDSAV
     C                   Z-ADD     *ZEROS        TTDSPT
     C                   Z-ADD     *ZEROS        TTTSAV
     C                   Z-ADD     *ZEROS        TTSVPT
     C*
     C                   END
     C                   MOVE      *BLANKS       XFIRST
     C*
     C* GET THE EMP# INFORMATION FOR THE HEADING
     C*
     C                   Z-ADD     PDEMP#        XXEMP#
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        XXEMNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXEMNM
     C                   END
     C*
     C                   Z-ADD     XXEMP#        HLEMP#
     C*
     C*
     C* PRINT HEADINGS
     C*
     C   15              EXCEPT    HEAD
     C   16              EXCEPT    HEAD1
     C*
     C                   ENDSR
     C*
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
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *************************************************
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
     O          E            ROUTE          2
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
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *************************************************
     O          E            ROUTE          2 33
     O                                           18 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O                                           26 'COMPANY        :'
     O                       XXCO#               31
     O          E            ROUTE          2
     O                                           26 'SUBSCRIBER     :'
     O               21      XXSUB#              33
     O              N21                          32 '*ALL'
     O          E            ROUTE          2
     O                                           26 'GROUP          :'
     O               21      XXFND               31
     O              N21                          32 '*ALL'
     O          E            ROUTE          2
     O                                           26 'EMPLOYER       :'
     O              N18 21   XXEMP#              35
     O               18                          32 '*ALL'
     O          E            ROUTE          2
     O                                           26 'PROVIDER       :'
     O               19      XXFEIN              37
     O               19      XXMOD               41
     O              N19                          32 '*ALL'
     O          E            ROUTE          2
     O                                           26 'CLOSING DATE RANGE :'
     O                       X1FRDT              38 '  /  /    '
     O                                           41 'TO'
     O                       X1TODT              52 '  /  /    '
     O          E            ROUTE          2
     O                                           26 'SELECTION      :'
     O               20                          37 'PROCEDURE'
     O              N20                          37 'DIAGNOSIS'
     O          E            ROUTE          2
     O                                           26 'TYPE OF ADMISSION :'
     O               22                          30 'M'
     O               23                          32 'S'
     O               24                          34 'Y'
     O               25                          36 'P'
     O               26                          38 'C'
     O               27                          32 '*ALL'
     O          E            ROUTE          2
     O                                           26 'REPORT TYPE    :'
     O               15N16                       34 'DETAIL'
     O               16N15                       35 'SUMMARY'
     O               15 16                       32 'BOTH'
     O          E            ROUTE          2
     O                                           26 'RUN DATE       :'
     O                       DSDATE              38 '  /  /    '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    127
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       XXTITL              96
     O                                          121 'PAGE:'
     O                       PAGE          Z    127
     O          E            HEAD           1
     O                                           62 'FOR CLOSING DATES:'
     O                       X1FRDT              73 '  /  /    '
     O                                           77 'TO'
     O                       X1TODT              88 '  /  /    '
     O          E            HEAD           2
     O                                           61 'BY '
     O               20                          70 'PROCEDURE'
     O              N20                          70 'DIAGNOSIS'
     O          E    21      HEAD           1
     O               21                          12 'SUBSCRIBER.:'
     O               21      XXSUB#              19
     O               21                          21 '-'
     O               21      XXSBNM              62
     O          E    21      HEAD           1
     O               21                          12 'GROUP......:'
     O               21      XXFND               19
     O               21                          21 '-'
     O               21      XXFNNM              82
     O          E    21      HEAD           2
     O               21                          12 'EMPLOYER...:'
     O               21      XXEMP#              23
     O               21                          25 '-'
     O               21      XXEMNM              66
     O          E    19      HEAD           2
     O               19                          12 'PROVIDER...:'
     O               19      XXFEIN              23
     O               19      XXMOD               27
     O               19                          29 '-'
     O               19      XXPDNM              70
     O          E            HEAD           1
     O                                           28 '-------------------'
     O                                           36 'APPROVED'
     O                                           55 '-------------------'
     O                                           63 '-----'
     O                                           69 'DENIED'
     O                                           74 '-----'
     O                                           83 'TOTAL'
     O          E            HEAD           1
     O                                           07 'PRIMARY'
     O                                           16 'TOTAL'
     O                                           25 'NUMBER'
     O                                           35 'TOT DAYS'
     O                                           45 'TOT DAYS'
     O                                           55 'TOT DAYS'
     O                                           64 'NUMBER'
     O                                           74 'TOT DAYS'
     O                                           84 'TOT DAYS'
     O          E            HEAD           2
     O                                           06 'CODE'
     O                                           17 'ACTIVITY'
     O                                           25 'CERTS'
     O                                           34 'REQUEST'
     O                                           43 'RECOMM'
     O                                           54 'SAVED'
     O                                           64 'CERTS'
     O                                           72 'SAVED'
     O                                           83 'SAVED'
     O          E            DET            1
     O               20      PDCPT4              06
     O*** Beg Chg *** 08/05/13 ****************************************
     O********      N20      PDICD9              06
     O              N20      XXICD               08
     O*** End Chg *** 08/05/13 ****************************************
     O                       PDTACT        Z     15
     O                       PDAPPR        Z     25
     O                       PDREQD        Z     32
     O                       PDRECD        Z     42
     O                       PDASAV        Z     53
     O                       PDDENY        Z     64
     O                       PDDSAV        Z     72
     O                       PDTSAV        Z     82
     O          E            TTEMP       2  1
     O               21                          05 'EMPLR'
     O               19                          05 'PROV'
     O          E            TTEMP          2
     O                                           07 'TOTALS:'
     O                       ETTACT        Z     15
     O                       ETAPPR        Z     25
     O                       ETREQD        Z     32
     O                       ETRECD        Z     42
     O                       ETASAV        Z     53
     O                       ETDENY        Z     64
     O                       ETDSAV        Z     72
     O                       ETTSAV        Z     82
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
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
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE1         1
     O**********     08      ORDIS2              60
     O**********E            ROUTE1         1
     O**********     08      ORDIS3              60
     O**********E            ROUTE1         2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *************************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE1         1
     O               08      ORDIS2              60
     O          E            ROUTE1         1
     O               08      ORDIS3              60
     O          E            ROUTE1         2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE1         2
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
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *************************************************
     O          E            ROUTE1         2 33
     O                                           18 'REPORT SELECTIONS:'
     O          E            ROUTE1         2
     O                                           26 'COMPANY        :'
     O                       XXCO#               31
     O          E            ROUTE1         2
     O                                           26 'SUBSCRIBER     :'
     O               21      XXSUB#              33
     O              N21                          32 '*ALL'
     O          E            ROUTE1         2
     O                                           26 'GROUP          :'
     O               21      XXFND               31
     O              N21                          32 '*ALL'
     O          E            ROUTE1         2
     O                                           26 'EMPLOYER       :'
     O              N18 21   XXEMP#              35
     O               18                          32 '*ALL'
     O          E            ROUTE1         2
     O                                           26 'PROVIDER       :'
     O               19      XXFEIN              37
     O               19      XXMOD               41
     O              N19                          32 '*ALL'
     O          E            ROUTE1         2
     O                                           26 'CLOSING DATE RANGE :'
     O                       X1FRDT              38 '  /  /    '
     O                                           41 'TO'
     O                       X1TODT              52 '  /  /    '
     O          E            ROUTE1         2
     O                                           26 'SELECTION      :'
     O               20                          37 'PROCEDURE'
     O              N20                          37 'DIAGNOSIS'
     O          E            ROUTE1         2
     O                                           26 'TYPE OF ADMISSION :'
     O               22                          30 'M'
     O               23                          32 'S'
     O               24                          34 'Y'
     O               25                          36 'P'
     O               26                          38 'C'
     O               27                          32 '*ALL'
     O          E            ROUTE1         2
     O                                           26 'REPORT TYPE    :'
     O               15N16                       34 'DETAIL'
     O               16N15                       35 'SUMMARY'
     O               15 16                       32 'BOTH'
     O          E            ROUTE1         2
     O                                           26 'RUN DATE       :'
     O                       DSDATE              38 '  /  /    '
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    127
     O          E            HEAD1          1
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          121 'PAGE:'
     O                       PAGE1         Z    127
     O          E            HEAD1          1
     O                                           62 'FOR CLOSING DATES:'
     O                       X1FRDT              73 '  /  /    '
     O                                           77 'TO'
     O                       X1TODT              88 '  /  /    '
     O          E            HEAD1          2
     O                                           61 'BY '
     O               20                          70 'PROCEDURE'
     O              N20                          70 'DIAGNOSIS'
     O          E    21      HEAD1          1
     O               21                          12 'SUBSCRIBER.:'
     O               21      XXSUB#              19
     O               21                          21 '-'
     O               21      XXSBNM              62
     O          E    21      HEAD1          1
     O               21                          12 'GROUP......:'
     O               21      XXFND               19
     O               21                          21 '-'
     O               21      XXFNNM              82
     O          E    21      HEAD1          2
     O               21                          12 'EMPLOYER...:'
     O               21      XXEMP#              23
     O               21                          25 '-'
     O               21      XXEMNM              66
     O          E    19      HEAD1          2
     O               19                          12 'PROVIDER...:'
     O               19      XXFEIN              19
     O               19      XXMOD               23
     O               19                          25 '-'
     O               19      XXPDNM              66
     O          E            HEAD1          1
     O                                           38 '--------------------'
     O                                           46 'APPROVED'
     O                                           66 '--------------------'
     O                                           79 '-----------'
     O                                           85 'DENIED'
     O                                           96 '-----------'
     O                                          107 'TOTAL'
     O          E            HEAD1          1
     O                                           07 'PRIMARY'
     O                                           16 'TOTAL'
     O                                           28 'CERTS'
     O                                           42 'TOT DAYS'
     O                                           52 'TOT DAYS'
     O                                           59 'DAYS'
     O                                           65 'SAV'
     O                                           78 'CERTS'
     O                                           93 'TOT DAYS'
     O                                          102 'TOT'
     O                                          110 'SAV'
     O          E            HEAD1          2
     O                                           06 'CODE'
     O                                           17 'ACTIVITY'
     O                                           22 '#'
     O                                           29 '%'
     O                                           41 'REQUEST'
     O                                           50 'RECOMM'
     O                                           57 '#'
     O                                           64 '%'
     O                                           72 '#'
     O                                           79 '%'
     O                                           86 '#'
     O                                           94 '%'
     O                                          101 '#'
     O                                          109 '%'
     O          E            DET1           1
     O               20      PDCPT4              06
     O*** Beg Chg *** 08/05/13 ****************************************
     O********      N20      PDICD9              06
     O              N20      XXICD               08
     O*** End Chg *** 08/05/13 ****************************************
     O                       PDTACT        Z     15
     O                       PDAPPR        Z     23
     O                       APPRPT        M     31
     O                       PDREQD        Z     40
     O                       PDRECD        Z     49
     O                       PDASAV        Z     58
     O                       ASAVPT        M     66
     O                       PDDENY        Z     73
     O                       DENTPT        M     81
     O                       PDDSAV        Z     88
     O                       DSAVPT        M     96
     O                       PDTSAV        Z    103
     O                       TSAVPT        M    111
     O          E            TTRPT       2  1
     O                                           06 'REPORT'
     O          E            TTRPT          2
     O                                           07 'TOTALS:'
     O                       TTTACT        Z     15
     O                       TTAPPR        Z     23
     O                       TTAPPT        M     31
     O                       TTREQD        Z     40
     O                       TTRECD        Z     49
     O                       TTASAV        Z     58
     O                       TTASPT        M     66
     O                       TTDENY        Z     73
     O                       TTDEPT        M     81
     O                       TTDSAV        Z     88
     O                       TTDSPT        M     96
     O                       TTTSAV        Z    103
     O                       TTSVPT        M    111
     O          E            ERRMS1      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
     O*
