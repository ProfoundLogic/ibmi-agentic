     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE F0151R - EOMB INVOICE/PAYMENT INFORMATION                *
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    PROGRAM TO DISPLAY INVOICES AND PAYMENTS THAT HAVE BEEN    *
     H*    PROCESSED FOR AN EOMB                                      *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    F0150R - EOMB DELETE FACILITY                              *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  PROGRAMMER: C:20               REQ/PROJ: 123/MC100           *
     H*  CREATED: 09/02/93                                            *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR01:                                                        *
     H*  50    DO WHILE/REQD EQUAL                                    *
     H*  60    GENERAL CHAINING                                       *
     H*  71    WCMC INTERFACE INSTALLED AND INSIDE ACCOUNT            *
     H*  74    NETWORK INVOICING INSTALLED                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/04/99 C:31  Project: MC152
     H*                 1. Change size of invoice number
     H*
     H*  03/08/99 C:94  PROJECT: MC048 SUP16553
     H*                 CHANGE PROGRAM SO THAT INDEX DOES NOT GO
     H*                 HIGHER THAN 3
     H*
     H*  06/24/98 C:53  REQ/PROJ: MC145                               *
     H*                 SWITCH OLD NETWORK/AR FLAG TO NEW A/R FLAG
     H*                                                               *
     H*  01/11/95 C:20   REQ/PROJ: 149/MC116                          *
     H*                  1. CHANGED LOGICAL USED TO CHAIN TO WDPAP    *
     H*                     FROM WDPAL4 TO WDPAL8.  THE NEW LOGICAL   *
     H*                     INCLUDES EOB# AT THE END.                 *
     H*                                                               *
     H*  05/03/96 C:53   REQ/PROJ: 3245/MC068                         *
     H*                  CHANGE FILE NMARL6 TO NMARLB WHICH HAS       *
     H*                  SELECT CRITERIA THAT THE PROGRAM NEEDS AND   *
     H*                  REMOVE CRITERIA EDITS FROM THE PROGRAM       *
     H*****************************************************************
     FF0151F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*
     FSMUPL     IF   E           K DISK
     FNMARLB    IF   E           K DISK
     FFAEOL3    IF   E           K DISK
     FSSYLF     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FWARPL     IF   E           K DISK    USROPN
     FWDPAL8    IF   E           K DISK    USROPN
     FFMPDL     IF   E           K DISK
     FFAE1L     IF   E           K DISK

     D*
     D*** BEGIN CHG *** 01/04/99 **************************************
     D********            IN          2  5 0             INVOICE #
     D*** END CHG *** 01/04/99 ****************************************
     D* INVOICE NUMBER
     D                 DS
     D*** BEGIN CHG *** 01/04/99 **************************************
     D********                                1  100IN
     D  IN                     1     18  0
     D                                     DIM(2)                               INVOICE #
     D********                                1   50SCIN01
     D  SCIN01                 1      9  0
     D********                                6  100SCIN02
     D  SCIN02                10     18  0
     D*** END CHG *** 01/04/99 ****************************************
     D* INVOICE DATE
     D                 DS
     D  DT                     1    016  0
     D                                     DIM(2)                               INVOICE DATE
     D  SCDT01               001    008  0
     D  SCDT02               009    016  0
     D* SYSTEM TYPE
     D                 DS
     D  SY                     1      2
     D                                     DIM(2)                               SYSTEM TYPE
     D  SCSY01                 1      1
     D  SCSY02                 2      2
     D* PAYMENT MADE TO
     D                 DS
     D  PY                     1     60
     D                                     DIM(3)                               PAY MADE TO
     D  SCPY01                 1     20
     D  SCPY02                21     40
     D  SCPY03                41     60
     D* CHECK NUMBER
     D                 DS
     D  CK                     1     21  0
     D                                     DIM(3)                               CHECK NUMBER
     D  SCCK01                 1      7  0
     D  SCCK02                 8     14  0
     D  SCCK03                15     21  0
     D* CHECK DATE
     D                 DS
     D  CD                     1    024  0
     D                                     DIM(3)                               CHECK DATE
     D  SCCD01               001    008  0
     D  SCCD02               009    016  0
     D  SCCD03               017    024  0
     D* AMOUNT OF CHECK
     D                 DS
     D  AM                     1     27  2
     D                                     DIM(3)                               CHECK AMOUNT
     D  SCAM01                 1      9  2
     D  SCAM02                10     18  2
     D  SCAM03                19     27  2
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  A/R FILE
     C*
     C     ARKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    XXEOB#
     C*
     C*  EOMB TRANSACTION FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXEOB#
     C*
     C*  EOMB AUXILIARY FILE
     C*
     C     E1KEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C*  FUND AUXILIARY FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C*
     C*  WCMS CHECK REQUEST FILE(WARPP)
     C*
     C     RPKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    FNWCF#
     C                   KFLD                    EOFYR
     C*
     C*  WCMS PAYMENT DETAIL FILE(WDPAP)
     C*
     C     PAKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    FNWCF#
     C                   KFLD                    XXCHK#
     C                   KFLD                    EOEOB#
     C*
     C*  PROVIDER MASTER FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FS BASE CONTROL FILE
     C*
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
     C*
     C*------------------------*
     C*   PARAMETER LIST FOR CALLED PROGRAM
     C*------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQEOB#            9
     C                   PARM                    QQRTCD            1
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
     C* INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOWCCK        XXCHK#
     C     *LIKE         DEFINE    PDFEIN        XXFEIN
     C     *LIKE         DEFINE    PDMOD         XXMOD
     C                   Z-ADD     *ZEROS        I1                5 0
     C                   Z-ADD     *ZEROS        I2                5 0
     C                   Z-ADD     *ZEROS        IN
     C                   Z-ADD     *ZEROS        DT
     C                   MOVE      *BLANKS       SY
     C                   Z-ADD     *ZEROS        CK
     C                   Z-ADD     *ZEROS        CD
     C                   Z-ADD     *ZEROS        AM
     C                   MOVE      *BLANKS       PY
     C                   MOVEL     'WDW01'       Q2RECF
     C*
     C* MOVE ENTRY PARMS TO WORKFIELDS
     C*
     C                   MOVEL     QQPRDS        SCPRDS
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQEOB#        XXEOB#
     C*
     C* VERIFY INSTALLED SYSTEMS INFORMATION FROM SYSTEM CONTROL FILE
     C*
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* CHECK TO SEE IF NETWORK/PPO INSTALLED
     C*
     C                   SUBST     FBAL04:5      XXAR              1            ADD 06/24/98
     C     XXAR          IFEQ      'Y'                                          ADD 06/24/98
     C********** FBFL06    IFEQ 'Y'                        DEL 06/24/98
     C                   MOVE      '1'           *IN74
     C                   ELSE
     C                   MOVE      '0'           *IN74
     C                   END                                                    END FBFL06
     C*
     C* CHECK TO SEE IF WCMS INTERFACE INSTALLED
     C*
     C     FBFL01        IFEQ      'Y'
     C                   MOVE      '1'           *IN71
     C                   OPEN      WARPL
     C                   OPEN      WDPAL8
     C                   ELSE
     C                   MOVE      '0'           *IN71
     C                   END                                                    END FBFL01
     C*
     C                   END                                                    END *IN60
     C*
     C* IF NETWORK/PPO AND WCMS INTERFACE ARE BOTH NOT INSTALLED END PROGRAM
     C*
     C     *IN71         IFEQ      '0'
     C     *IN74         ANDEQ     '0'
     C                   GOTO      TERM
     C                   END                                                    END *IN71/*IN74
     C*
     C* CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZEROS        UPCO#
     C                   END                                                    END *IN60 UP
     C*
     C* CHAIN TO EOMB TRANSACTION FILE TO GET EOMB INFORMATION
     C*
     C     EOKEY         CHAIN     PEO01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* MOVE EO FILE FIELDS TO SCREEN FIELDS
     C*
     C                   Z-ADD     EOSUB#        SCSUB#
     C                   Z-ADD     EOFND         SCFND
     C                   Z-ADD     EOFYR         SCFYR
     C                   Z-ADD     EOCASE        SCCASE
     C                   Z-ADD     EOEOB#        SCEOB#
     C*
     C* CHAIN TO FUND AUXILIARY FILE TO LOOK FOR OUTSIDE OR INSIDE ACCOUNT
     C*
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      '0'
     C*
     C* VERIFY INSIDE/OUTSIDE ACCOUNT PROCESSING FLAG FOR THIS GROUP
     C*
     C     F1OUTA        IFEQ      'Y'
     C                   MOVE      '0'           *IN71
     C                   END                                                    END F1OUTA
     C*
     C                   END                                                    END *IN60 F1
     C*
     C* READ THE A/R FILE TO SEE IF THIS EOMB HAS BEEN APPLIED TO AN
     C* INVOICE
     C                   Z-ADD     *ZEROS        I1
     C     ARKEY         CHAIN     PAR01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C*** BEG ADD *** 03/08/99 **************************************
     C     I1            IFLT      3
     C*** END ADD *** 03/08/99 **************************************
     C                   ADD       1             I1
     C                   Z-ADD     ARINV#        IN(I1)
     C*
     C                   Z-ADD     ARTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C*
     C                   MOVE      ARSYS         SY(I1)
     C*** BEG ADD *** 03/08/99 **************************************
     C                   ENDIF
     C*** END ADD *** 03/08/99 **************************************
     C*
     C     ARKEY         READE     PAR01                                  50
     C                   END                                                    END *IN50 AR
     C*
     C* IF WCMS INTERFACE INSTALLED AND THIS IS AN INSIDE ACCOUNT (*IN71=1)
     C* CHAIN TO WCMC FUND MASTER FILE TO GET WCMS XREF FUND NUMBER
     C*
     C     *IN71         IFEQ      '1'
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* CHAIN TO THE WCMS REQUEST FOR PAYMENT FILE TO SEE IF ANY CHECK
     C* REQUESTS FOR THIS EOMB HAVE BEEN PROCESSED
     C*
     C                   Z-ADD     *ZERO         I2
     C     RPKEY         CHAIN     PRP01                              50
     C     *IN50         DOWEQ     '0'
     C*** BEG ADD *** 03/08/99 **************************************
     C     I2            IFLT      3
     C*** END ADD *** 03/08/99 **************************************
     C*
     C* VERIFY RPEOB# IS THE SAME AS EOEOB#
     C*
     C     RPEOB#        IFEQ      EOEOB#
     C*
     C                   ADD       1             I2
     C                   Z-ADD     RPCHK#        CK(I2)
     C                   Z-ADD     RPAMT         AM(I2)
     C*
     C                   Z-ADD     RPTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CD(I2)
     C*
     C* CHAIN TO PROVIDER FILE TO FIND NAME TO WHOM THE PAYMENT WAS MADE
     C*
     C                   Z-ADD     RPFEIN        XXFEIN
     C                   Z-ADD     RPMOD         XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END                                                    END NE *BLANK
     C                   MOVEL     XXLAST        PY(I2)
     C                   ELSE                                                   ELSE *IN60 PD
     C                   MOVE      *BLANKS       PY(I2)
     C*
     C                   END                                                    END *IN60 PD
     C*
     C                   END                                                    END RPEOB#
     C*** BEG ADD *** 03/08/99 **************************************
     C                   ENDIF                                                  I2 < 3
     C*** END ADD *** 03/08/99 **************************************
     C*
     C     RPKEY         READE     PRP01                                  50
     C                   END                                                    END *IN50 RP
     C*
     C* CHAIN TO THE WCMS PAYMENT DETAIL FILE TO SEE IF ANY CHECK
     C* FOR THIS EOMB HAVE BEEN PROCESSED FOR THE PROVIDER
     C*
     C     EOWCCK        IFNE      *ZEROS
     C                   Z-ADD     EOWCCK        XXCHK#
     C*
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      '0'
     C*** BEG ADD *** 03/08/99 **************************************
     C     I2            IFLT      3
     C*** END ADD *** 03/08/99 **************************************
     C*
     C                   ADD       1             I2
     C                   Z-ADD     PACHK#        CK(I2)
     C                   Z-ADD     PAAMT         AM(I2)
     C*
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CD(I2)
     C*
     C* CHAIN TO PROVIDER FILE TO FIND NAME TO WHOM THE PAYMENT WAS AMDE
     C*
     C                   Z-ADD     PAFEIN        XXFEIN
     C                   Z-ADD     PAMOD         XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END                                                    END NE *BLANK
     C                   MOVEL     XXLAST        PY(I2)
     C                   ELSE                                                   ELSE *IN60 PD
     C                   MOVE      *BLANKS       PY(I2)
     C*
     C                   END                                                    END *IN60 PD
     C*** BEG ADD *** 03/08/99 **************************************
     C                   ENDIF                                                  I2 < 3
     C*** END ADD *** 03/08/99 **************************************
     C*
     C                   END                                                    END *IN60 PA
     C*
     C                   END                                                    END EOWCCK
     C*
     C* CHAIN TO EOMB AUXILIARY FILE TO SEE IF ANY OTHER CHECKS FOR THIS
     C* EOMB HAVE BEEN PROCESSED
     C*
     C     E1KEY         CHAIN     PE101                              60
     C     *IN60         IFEQ      '0'
     C*
     C* IF AN OUTSIDE NETWORK CHECK HAS BEEN PROCESSED GET THE NAME TO
     C* WHOM THE CHECK WAS PROCESSED FOR
     C*
     C     E110A1        IFNE      *BLANKS
     C                   MOVEL     E110A1        XXCHK#
     C*
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*** BEG ADD *** 03/08/99 **************************************
     C     I2            IFLT      3
     C*** END ADD *** 03/08/99 **************************************
     C                   ADD       1             I2
     C                   Z-ADD     PACHK#        CK(I2)
     C                   Z-ADD     PAAMT         AM(I2)
     C*
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CD(I2)
     C*
     C* CHAIN TO PROVIDER FILE TO FIND NAME TO WHOM THE PAYMENT WAS AMDE
     C*
     C                   Z-ADD     PAFEIN        XXFEIN
     C                   Z-ADD     PAMOD         XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END                                                    END NE *BLANK
     C                   MOVEL     XXLAST        PY(I2)
     C                   ELSE                                                   ELSE *IN60 PD
     C                   MOVE      *BLANKS       PY(I2)
     C*
     C                   END                                                    END *IN60 PD
     C*
     C*** BEG ADD *** 03/08/99 **************************************
     C                   ENDIF                                                  I2 < 3
     C*** END ADD *** 03/08/99 **************************************
     C                   END                                                    END *IN60 PA
     C*
     C                   END                                                    END E110A1
     C*
     C* IF THE BILL REVIEW CHECK HAS BEEN PROCESSED CHAIN TO PROVDIER
     C* FILE TO GET NAME TO WHOM CHECK WAS PROCESSED
     C*
     C     E110A2        IFNE      *BLANKS
     C                   MOVEL     E110A2        XXCHK#
     C*
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*** BEG ADD *** 03/08/99 **************************************
     C     I2            IFLT      3
     C*** END ADD *** 03/08/99 **************************************
     C                   ADD       1             I2
     C                   Z-ADD     PACHK#        CK(I2)
     C                   Z-ADD     PAAMT         AM(I2)
     C*
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CD(I2)
     C*
     C* CHAIN TO PROVIDER FILE TO FIND NAME TO WHOM THE PAYMENT WAS AMDE
     C*
     C                   Z-ADD     PAFEIN        XXFEIN
     C                   Z-ADD     PAMOD         XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END                                                    END NE *BLANK
     C                   MOVEL     XXLAST        PY(I2)
     C                   ELSE                                                   ELSE *IN60 PD
     C                   MOVE      *BLANKS       PY(I2)
     C*
     C                   END                                                    END *IN60 PD
     C*
     C*** BEG ADD *** 03/08/99 **************************************
     C                   ENDIF                                                  I2 < 3
     C*** END ADD *** 03/08/99 **************************************
     C                   END                                                    END *IN60 PA
     C*
     C                   END                                                    END E110A2
     C*
     C                   END                                                    END *IN60 E1
     C*
     C                   END                                                    END *IN60 FN
     C*
     C                   END                                                    END *IN71
     C*
     C                   END                                                    END *IN60 EO
     C*
     C* IF WE HAVE FOUND ANY INVOICES OR ANY CHECK REQUESTS WE WILL
     C* DISPLAY THE WINDOW HERE ELSE WE WILL END THE PROGRAM
     C*
     C     I1            IFEQ      0
     C     I2            ANDEQ     0
     C     QQCALL        ANDEQ     *BLANKS
     C                   GOTO      TERM
     C                   ELSE                                                   ELSE I1/I2
     C     Q2RECF        IFEQ      'WDW01'
     C                   EXFMT     WDW01
     C                   END                                                    END Q2RECF
     C                   END                                                    END I1/I2
     C*===============================================================
     C* TERMINATION OF PROGRAM
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C                   MOVE      '*'           QQRTCD
     C*
     C/EJECT
