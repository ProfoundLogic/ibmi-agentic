     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR  dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE U7070R - PROVIDER ANALYSIS REPORT - PRT PGM
     F*
     H*****************************************************************
     H*  INDICATORS:                                                  *
     H*  08    Batch Mode                                             *
     H*  09    Demand Mode  1                                         *
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
     H* 01/23/13 C:287 Project: SY266_QRY                             *
     H*                Add DFTACTGRP and ACTGRP compile options       *
     H*                                                               *
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
     FUXPAL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMEML     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOF)
     F*

     D                 DS
     D*** BEGIN CHG *** 03/14/00 **************************************
     D**********                              1  120DSDTTM
     D**********                              1   60DSTIME
     D**********                              7  120DSDATE
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*** END   CHG *** 03/14/00 **************************************
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

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
     C* USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
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
     C*   KEY LIST FOR THE EMPLOYER
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*   KEY LIST FOR THE PROVIDER
     C*
     C     PDKEY         KLIST
     C                   KFLD                    PAFEIN
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C*
     C* INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    PACO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    PASUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    PAFND         XXFND                          3.0N
     C     *LIKE         DEFINE    PAEMP#        XXEMP#                         9.0N
     C     *LIKE         DEFINE    SMNAME        XXSBNM                         40A
     C     *LIKE         DEFINE    ORTITL        XXTITL                         40A
     C     *LIKE         DEFINE    EMNAME        XXEMNM                         40A
     C                   MOVE      *BLANKS       XXTYPE            1            1A
     C                   MOVE      *BLANKS       XXPRT             1            1A
     C                   MOVE      *BLANKS       XXPRNM           20            20A
     C                   MOVE      *BLANKS       XXFNNM           60            60A
     C                   Z-ADD     *ZEROS        XXTSRV            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXAPCT            5 2          5.2N
     C                   Z-ADD     *ZEROS        XXPPCT            5 2          5.2N
     C                   Z-ADD     *ZEROS        XXDPCT            5 2          5.2N
     C                   Z-ADD     *ZEROS        XXRDPT            5 2          5.2N
     C                   Z-ADD     *ZEROS        XXRPCT            5 2          5.2N
     C                   Z-ADD     *ZEROS        XXSPCT            5 2          5.2N
     C                   Z-ADD     *ZEROS        XXTPCT            5 2          5.2N
     C                   Z-ADD     *ZEROS        TTAPPR            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTAPP            5 0          5.0N
     C                   Z-ADD     *ZEROS        TTPEND            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTPND            5 0          5.0N
     C                   Z-ADD     *ZEROS        TTDENY            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTDNY            5 0          5.0N
     C                   Z-ADD     *ZEROS        TTRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        TTRCRT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTRCR            5 0          5.0N
     C                   Z-ADD     *ZEROS        TTTOTA            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTTOT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TTRQDY            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTRQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        TTCTDY            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTCTD            5 0          5.0N
     C                   Z-ADD     *ZEROS        TTSVDY            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTSVD            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTAPC            5 2          5.2N
     C                   Z-ADD     *ZEROS        TOTPPC            5 2          5.2N
     C                   Z-ADD     *ZEROS        TOTDPC            5 2          5.2N
     C                   Z-ADD     *ZEROS        TOTRDP            5 2          5.2N
     C                   Z-ADD     *ZEROS        TOTRPC            5 2          5.2N
     C                   Z-ADD     *ZEROS        TOTSPC            5 2          5.2N
     C                   Z-ADD     *ZEROS        TOTTPC            5 2          5.2N
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
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
     C*
     C     XXEMP#        IFNE      *ZEROS
     C                   MOVE      '1'           *IN18
     C                   MOVE      '1'           *IN19
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
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
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
      ***** Beg Del ***** 08/26/14 *************************************************
     C**********15              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *************************************************
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
      ***** Beg Del ***** 08/26/14 *************************************************
     C********16              EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 *************************************************
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
     C                   ELSE
     C                   MOVE      *BLANKS       XXEMNM
     C                   END
     C*
     C* PRINT HEADINGS
     C*
     C   15              EXCEPT    HEAD
     C   16              EXCEPT    HEAD1
     C   18
     CAN 15              EXCEPT    HEAD2
     C*
     C* PROCESS THE CORRECT LOGICAL
     C*
     C                   READ      PPA01                                  50
     C*
     C* IF NO RECORDS PRINT ERROR MESSAGE
     C*
     C     *IN50         IFEQ      '1'
     C   15              EXCEPT    ERRMSG
     C   16              EXCEPT    ERRMS1
     C                   GOTO      TERM
     C                   END                                                    END *IN60
     C*
     C* START LOOP
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C* IF THIS IS THE FIRST TIME THROUGH AND REPORT IS FOR ALL EMPS
     C* GET THE FIRST ONE NOW
     C*
     C     XXEMP#        IFEQ      *ZEROS
     C     *IN19         ANDEQ     '0'
     C                   MOVEL     PAEMP#        XXEMP#
     C                   MOVE      '1'           *IN19
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        XXEMNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXEMNM
     C                   END
     C   15              EXCEPT    HEAD2
     C                   END
     C*
     C* IF THE EMP# CHANGES PRINT TOTALS AND GET NEXT INTR NAME FOR
     C* HEADING
     C*
     C     PAEMP#        IFNE      XXEMP#
     C                   EXSR      SREMP#
     C   15              EXCEPT    HEAD2
     C                   END                                                    END PCINTR
     C*
     C* GET THE PROVIDER NAME FOR REPORT
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXPRNM
     C                   ELSE
     C                   MOVEL     PDNAME        XXPRNM
     C                   END                                                    END CDFSN
     C                   END                                                    END *IN60
     C*
     C* ACCUMULATE THE EMPLOYER TOTAL AMOUNTS
     C*
     C                   ADD       PAAPPR        TTAPPR
     C                   ADD       PAPEND        TTPEND
     C                   ADD       PADENY        TTDENY
     C                   ADD       PAREDC        TTRECD
     C                   ADD       PARCRT        TTRCRT
     C                   ADD       PATOTA        TTTOTA
     C                   ADD       PARQDY        TTRQDY
     C                   ADD       PACTDY        TTCTDY
     C                   ADD       PASVDY        TTSVDY
     C*
     C* PRINT THE DETAIL LINE
     C*
     C     *INOA         IFEQ      '1'
     C   15              EXCEPT    HEAD
     C   15              EXCEPT    HEAD2
     C                   MOVE      '0'           *INOA
     C                   END
     C   15              EXCEPT    DET
     C*
     C* CALCULATE THE PERCENTAGES FOR THE SUMMARY REPORT
     C*
     C     PAAPPR        IFGT      *ZEROS
     C     PATOTA        ANDGT     *ZEROS
     C     PAAPPR        DIV       PATOTA        XXAPCT
     C     XXAPCT        MULT      100           XXAPCT
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXAPCT
     C                   END
     C*
     C     PAPEND        IFGT      *ZEROS
     C     PATOTA        ANDGT     *ZEROS
     C     PAPEND        DIV       PATOTA        XXPPCT
     C     XXPPCT        MULT      100           XXPPCT
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXPPCT
     C                   END
     C*
     C     PADENY        IFGT      *ZEROS
     C     PATOTA        ANDGT     *ZEROS
     C     PADENY        DIV       PATOTA        XXDPCT
     C     XXDPCT        MULT      100           XXDPCT
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXDPCT
     C                   END
     C*
     C     PAREDC        IFGT      *ZEROS
     C     PATOTA        ANDGT     *ZEROS
     C     PAREDC        DIV       PATOTA        XXRDPT
     C     XXRDPT        MULT      100           XXRDPT
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXRDPT
     C                   END
     C*
     C     PARCRT        IFGT      *ZEROS
     C     PATOTA        ANDGT     *ZEROS
     C     PARCRT        DIV       PATOTA        XXRPCT
     C     XXRPCT        MULT      100           XXRPCT
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXRPCT
     C                   END
     C*
     C     PACTDY        IFGT      *ZEROS
     C     PASVDY        ANDGT     *ZEROS
     C     PASVDY        DIV       PACTDY        XXSPCT
     C     XXSPCT        MULT      100           XXSPCT
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXSPCT
     C                   END
     C*
     C     PATOTA        IFGT      *ZEROS
     C     PATOTA        DIV       PATOTA        XXTPCT
     C     XXTPCT        MULT      100           XXTPCT
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXTPCT
     C                   END
     C*
     C   16              EXCEPT    DET1
     C*
     C* GET THE NEXT PROVIDER ANALYSIS REPORT WORKFILE RECORD
     C*
     C                   READ      PPA01                                  50
     C*
     C                   END                                                    END DOW 50
     C*
     C* PRINT TOTAL FOR THE LAST RECORD
     C*
     C     *INOA         IFEQ      '1'
     C   15              EXCEPT    HEAD
     C   15              EXCEPT    HEAD2
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXSR      SREMP#
     C   15              EXCEPT    TOTRP
     C   16              EXCEPT    TOT1
     C*
     C* END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found and (xxtype= 'B' or xxtype = 'D');
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
     C* SREMP# - ACCUMULATE TOTALS, PRINT TOTALS, GET NEXT EMP#
     C*****************************************************************
     C     SREMP#        BEGSR
     C*
     C* PRINT THE EMPLOYER TOTALS
     C*
     C   15              EXCEPT    TOT
     C*
     C     *IN16         IFEQ      '1'
     C                   ADD       TTAPPR        TOTAPP
     C                   ADD       TTPEND        TOTPND
     C                   ADD       TTDENY        TOTDNY
     C                   ADD       TTRECD        TOTREC
     C                   ADD       TTRCRT        TOTRCR
     C                   ADD       TTTOTA        TOTTOT
     C                   ADD       TTRQDY        TOTRQD
     C                   ADD       TTCTDY        TOTCTD
     C                   ADD       TTSVDY        TOTSVD
     C*
     C* CALCULATE THE PERCENTAGES FOR THE SUMMARY REPORT
     C*
     C     TOTAPP        IFGT      *ZEROS
     C     TOTTOT        ANDGT     *ZEROS
     C     TOTAPP        DIV       TOTTOT        TOTAPC
     C     TOTAPC        MULT      100           TOTAPC
     C                   ELSE
     C                   Z-ADD     *ZEROS        TOTAPC
     C                   END
     C*
     C     TOTPND        IFGT      *ZEROS
     C     TOTTOT        ANDGT     *ZEROS
     C     TOTPND        DIV       TOTTOT        TOTPPC
     C     TOTPPC        MULT      100           TOTPPC
     C                   ELSE
     C                   Z-ADD     *ZEROS        TOTPPC
     C                   END
     C*
     C     TOTDNY        IFGT      *ZEROS
     C     TOTTOT        ANDGT     *ZEROS
     C     TOTDNY        DIV       TOTTOT        TOTDPC
     C     TOTDPC        MULT      100           TOTDPC
     C                   ELSE
     C                   Z-ADD     *ZEROS        TOTDPC
     C                   END
     C*
     C     TOTREC        IFGT      *ZEROS
     C     TOTTOT        ANDGT     *ZEROS
     C     TOTREC        DIV       TOTTOT        TOTRDP
     C     TOTRDP        MULT      100           TOTRDP
     C                   ELSE
     C                   Z-ADD     *ZEROS        TOTRDP
     C                   END
     C*
     C     TOTRCR        IFGT      *ZEROS
     C     TOTTOT        ANDGT     *ZEROS
     C     TOTRCR        DIV       TOTTOT        TOTRPC
     C     TOTRPC        MULT      100           TOTRPC
     C                   ELSE
     C                   Z-ADD     *ZEROS        TOTRPC
     C                   END
     C*
     C     TOTCTD        IFGT      *ZEROS
     C     TOTSVD        ANDGT     *ZEROS
     C     TOTSVD        DIV       TOTCTD        TOTSPC
     C     TOTSPC        MULT      100           TOTSPC
     C                   ELSE
     C                   Z-ADD     *ZEROS        TOTSPC
     C                   END
     C*
     C     TOTTOT        IFGT      *ZEROS
     C     TOTTOT        DIV       TOTTOT        TOTTPC
     C     TOTTPC        MULT      100           TOTTPC
     C                   ELSE
     C                   Z-ADD     *ZEROS        TOTTPC
     C                   END
     C*
     C                   END
     C                   Z-ADD     *ZEROS        TTAPPR
     C                   Z-ADD     *ZEROS        TTPEND
     C                   Z-ADD     *ZEROS        TTDENY
     C                   Z-ADD     *ZEROS        TTRECD
     C                   Z-ADD     *ZEROS        TTRCRT
     C                   Z-ADD     *ZEROS        TTTOTA
     C                   Z-ADD     *ZEROS        TTRQDY
     C                   Z-ADD     *ZEROS        TTCTDY
     C                   Z-ADD     *ZEROS        TTSVDY
     C                   Z-ADD     *ZEROS        XXAPCT
     C                   Z-ADD     *ZEROS        XXPPCT
     C                   Z-ADD     *ZEROS        XXDPCT
     C                   Z-ADD     *ZEROS        XXRDPT
     C                   Z-ADD     *ZEROS        XXRPCT
     C                   Z-ADD     *ZEROS        XXSPCT
     C*
     C* GET THE NEXT EMPLOYER NAME
     C*
     C                   MOVE      *BLANKS       XXEMP#
     C                   Z-ADD     PAEMP#        XXEMP#
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        XXEMNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXEMNM
     C                   END
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
     O                       XXSUB#              33
     O          E            ROUTE          2
     O                                           26 'GROUP          :'
     O                       XXFND               31
     O          E            ROUTE          2
     O                                           26 'EMPLOYER       :'
     O*** BEGIN CHG *** 03/14/00 **************************************
     O**********       18      XXEMP#    31
     O               18      XXEMP#              37
     O*** END   CHG *** 03/14/00 **************************************
     O              N18                          32 '*ALL'
     O          E            ROUTE          2
     O                                           26 'REPORT TYPE    :'
     O               15N16                       34 'DETAIL'
     O               16N15                       35 'SUMMARY'
     O               15 16                       32 'BOTH'
     O          E            ROUTE          2
     O                                           26 'RUN DATE       :'
     O*** BEGIN CHG *** 03/14/00 **************************************
     O**********               DSDATE    36 '  /  /  '
     O                       DSDATE              38 '  /  /    '
     O*** END   CHG *** 03/14/00 **************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O****                               20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                57 'INC. 2000      '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              97
     O                       DSDATE        Y    127
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       XXTITL              96
     O                                          121 'PAGE:'
     O                       PAGE          Z    127
     O          E            HEAD           1
     O                                           12 'SUBSCRIBER.:'
     O                       XXSUB#              19
     O                                           21 '-'
     O                       XXSBNM              62
     O          E            HEAD           1
     O                                           12 'GROUP......:'
     O                       XXFND               19
     O                                           21 '-'
     O                       XXFNNM              82
     O          E            HEAD2          2
     O                                           12 'EMPLOYER...:'
     O*** BEGIN CHG *** 03/14/00 **************************************
     O**********               XXEMP#    19
     O**********                         21 '-'
     O**********               XXEMNM    62
     O                       XXEMP#              23
     O                                           25 '-'
     O                       XXEMNM              66
     O*** END   CHG *** 03/14/00 **************************************
     O          E            HEAD2          1
     O                                           42 '-- LIABILITY FLAG --'
     O                                           69 'TOTAL'
     O                                           80 'TOT DAYS'
     O                                           90 'TOT DAYS'
     O                                          100 'TOT DAYS'
     O          E            HEAD2          2
     O                                           08 'PROVIDER'
     O                                           26 'APPR'
     O                                           33 'PEND'
     O                                           42 'DENIED'
     O                                           51 'REDUCED'
     O                                           60 'RE-CERT'
     O                                           70 'ACTIVITY'
     O                                           79 'REQUEST'
     O                                           90 'RECOMMEND'
     O                                           97 'SAVED'
     O          E            DET            1
     O                       XXPRNM              20
     O                       PAAPPR        Z     27
     O                       PAPEND        Z     34
     O                       PADENY        Z     41
     O                       PAREDC        Z     49
     O                       PARCRT        Z     58
     O                       PATOTA        Z     67
     O                       PARQDY        Z     77
     O                       PACTDY        Z     87
     O                       PASVDY        Z     97
     O          E            TOT         2  2
     O                                           14 'EMPLR TOTALS:'
     O                       TTAPPR        Z     27
     O                       TTPEND        Z     34
     O                       TTDENY        Z     41
     O                       TTRECD        Z     49
     O                       TTRCRT        Z     58
     O                       TTTOTA        Z     67
     O                       TTRQDY        Z     77
     O                       TTCTDY        Z     87
     O                       TTSVDY        Z     97
     O          E            TOTRP          2
     O                                           14 'REPORT TOTALS:'
     O                       TOTAPP        Z     27
     O                       TOTPND        Z     34
     O                       TOTDNY        Z     41
     O                       TOTREC        Z     49
     O                       TOTRCR        Z     58
     O                       TOTTOT        Z     67
     O                       TOTRQD        Z     77
     O                       TOTCTD        Z     87
     O                       TOTSVD        Z     97
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
     O                       XXSUB#              33
     O          E            ROUTE1         2
     O                                           26 'GROUP          :'
     O                       XXFND               31
     O          E            ROUTE1         2
     O                                           26 'EMPLOYER       :'
     O*** BEGIN CHG *** 03/14/00 **************************************
     O**********       18      XXEMP#    31
     O               18      XXEMP#              37
     O*** END   CHG *** 03/14/00 **************************************
     O              N18                          32 '*ALL'
     O          E            ROUTE1         2
     O                                           26 'REPORT TYPE    :'
     O               15N16                       34 'DETAIL'
     O               16N15                       35 'SUMMARY'
     O               15 16                       32 'BOTH'
     O          E            ROUTE1         2
     O                                           26 'RUN DATE       :'
     O*** BEGIN CHG *** 03/14/00 **************************************
     O**********               DSDATE    36 '  /  /  '
     O                       DSDATE              38 '  /  /    '
     O*** END   CHG *** 03/14/00 **************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE1
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                57 'INC. 2000      '
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              97
     O                       DSDATE        Y    152
     O          E            HEAD1          1
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          146 'PAGE:'
     O                       PAGE          Z    152
     O          E            HEAD1          2
     O                                           67 'STATISTICS FOR ALL'
     O                                           77 'EMPLOYERS'
     O          E            HEAD1          1
     O                                           12 'SUBSCRIBER.:'
     O                       XXSUB#              19
     O                                           21 '-'
     O                       XXSBNM              62
     O          E            HEAD1          2
     O                                           12 'GROUP......:'
     O                       XXFND               19
     O                                           21 '-'
     O                       XXFNNM              82
     O          E            HEAD1          1
     O                                           46 '-----------    LIABILITY'
     O                                           66 ' FLAG    -----------'
     O                                          104 'TOTAL'
     O                                          122 'TOT DAYS'
     O                                          138 'TOT DAYS'
     O                                          152 'TOT DAYS'
     O          E            HEAD1          1
     O                                           08 'PROVIDER'
     O                                           30 'APPR'
     O                                           45 'PEND'
     O                                           61 'DENIED'
     O                                           77 'REDUCED'
     O                                           92 'RE-CERT'
     O                                          107 'ACTIVITY'
     O                                          121 'REQUEST'
     O                                          137 'RECOMMEND'
     O                                          150 'SAVED'
     O          E            HEAD1          2
     O                                           25 '#'
     O                                           32 '%'
     O                                           40 '#'
     O                                           47 '%'
     O                                           55 '#'
     O                                           62 '%'
     O                                           70 '#'
     O                                           77 '%'
     O                                           85 '#'
     O                                           92 '%'
     O                                          100 '#'
     O                                          106 '%'
     O                                          114 '#'
     O                                          129 '#'
     O                                          144 '#'
     O                                          151 '%'
     O          E            DET1           1
     O                       XXPRNM              20
     O                       PAAPPR        Z     27
     O                       XXAPCT        M     35
     O                       PAPEND        Z     42
     O                       XXPPCT        M     50
     O                       PADENY        Z     57
     O                       XXDPCT        M     65
     O                       PAREDC        Z     72
     O                       XXRDPT        M     80
     O                       PARCRT        Z     87
     O                       XXRPCT        M     95
     O                       PATOTA        Z    102
     O                       XXTPCT        M    110
     O                       PARQDY        Z    116
     O                       PACTDY        Z    131
     O                       PASVDY        Z    146
     O                       XXSPCT        M    154
     O          E            TOT1        1  1
     O                                           14 'REPORT TOTALS:'
     O                       TOTAPP        Z     27
     O                       TOTAPC        M     35
     O                       TOTPND        Z     42
     O                       TOTPPC        M     50
     O                       TOTDNY        Z     57
     O                       TOTDPC        M     65
     O                       TOTREC        Z     72
     O                       TOTRDP        M     80
     O                       TOTRCR        Z     87
     O                       TOTRPC        M     95
     O                       TOTTOT        Z    102
     O                       TOTTPC        M    110
     O                       TOTRQD        Z    116
     O                       TOTCTD        Z    131
     O                       TOTSVD        Z    146
     O                       TOTSPC        M    154
     O          E            ERRMS1      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
     O*
