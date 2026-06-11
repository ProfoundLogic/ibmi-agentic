     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F8264R - CONCENTRA Paid Date File Program
     H*
     H*****************************************************************
     H*  Description - Populates the CONCENTRA paid Date File (FAN4P) *
     H*                                                               *
     H*  Called from: F8264C - CONCENTRA Paid Date File-Override Pgm  *
     H*  Calls to   : None                                            *
     H*                                                               *
     H*****************************************************************
     H*  Created on 08/08/2005 by c:79 for Project 135MET from clt129 *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H* 11/02/07  C:47   Project: 135MKL
     H*                  Add fields to DATA data structure.
     H*                                                               *
     H* 02/02/07  C:182  Project: MC275
     H*                  Check Mailing date
     H*
     H*****************************************************************
     FFAEAL17   IF   E           K DISK
     FFAN4P     O    E             DISK
     FFAN0LFX   IF   E           K DISK
     F*
      /COPY SCOPYWW01D
     D*
     D***PY SCOPYWW01I
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
    D*** BEG CHG *** 11/02/07 ****************************************
     D***                                     1  33 DATA
     D  DATA                   1     96
    D*** END CHG *** 11/02/07 ****************************************
     D  XXBLID                 1     23
     D  XXWCPD                26     33
    D*** BEG ADD *** 11/02/07 ****************************************
     D  XXCK#                 34     63
     D  XXWCPC                57     63
     D  XXCKAM                64     96
     D  XXAMT                 85     93
     D  XXDECP                94     94
     D  XXCENT                95     96
     D  XXP96                 96     96
     D  XXPAID                85     96
     D                 DS                  INZ
     D  X3PAID                 1     11  2
     D  X3AMT                  1      9
     D  X3CENT                10     11
     D*
     D XXA             C                   CONST('JKLMNOPQR}')
     D XXN             C                   CONST('1234567890')

    I*** END ADD *** 11/02/07 ****************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@DATE            8
     C                   PARM                    @@DCID           12
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDYY'       SRFFMT            8
     C                   PARM      '*MDY'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
    C*** BEG ADD *** 02/02/07 ****************************************
     C     DT3MDY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
    C*** END ADD *** 02/02/07 ****************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     N0KY          KLIST
     C                   KFLD                    KKBLID
     C*
     C*================================================================
     C*  LIKE defines
     C*================================================================
     C     *LIKE         DEFINE    U0BLID        KKBLID
     C*
    C*** BEG ADD *** 02/02/07 ****************************************
     C*
     C* EXECUTE PGMSTR SUBROUTINE
     C*
     C                   EXSR      PGMSTR
     C*
    C*** END ADD *** 02/02/07 ****************************************
     C*                                                               C
     C*
     C* EXECUTE MAIN SUBROUTINE
     C*
     C                   EXSR      MAINSR
     C*
     C*================================================================
     C* MAINSR - MAIN SUBROUTINE
     C*================================================================
     C     MAINSR        BEGSR
     C*
     C* Let's start reading thru these checks
     C*
     C                   Z-ADD     *ZEROS        COUNT             9 0
     C*
     C                   READ      PEA01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  EASINV        KKBLID
     C     N0KY          CHAIN     PU001                              99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      WRTDTA
     C                   ENDIF
     C*
     C                   READ      PEA01                                  99
     C                   ENDDO
     C*
     C                   EXSR      PGMEND
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of Program Processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVEL     *BLANKS       @@DCID
     C     COUNT         IFGT      *ZEROS
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         DATE              6
     C     'PD'          CAT(P)    DATE:0        @@DCID
     C                   CAT       '.DAT':0      @@DCID
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDTA - Write the Data Record
     C*================================================================
     C     WRTDTA        BEGSR
     C*
     C                   CLEAR                   PN401
     C                   RESET                   DATA
     C*
     C                   MOVEL     EASINV        XXBLID
    C*** BEG ADD *** 02/02/07 ****************************************
     C*
     C*USE THE CHECK MAILING DATE TO REPORT DATE PAID IF NOT BLANK
     C*
     C                   EXSR      GETMDT
     C*
     C*
     C     #1CMDT        IFNE      *BLANKS
     C*
     C                   MOVE      #1CMDT        XXWCPD
     C*
     C                   ELSE
    C*** END ADD *** 02/02/07 ****************************************
     C*
     C                   MOVE      EAWCPD        XXWCPD
     C*
    C*** BEG ADD *** 02/02/07 ****************************************
     C*
     C                   ENDIF
     C*
    C*** END ADD *** 02/02/07 ****************************************
    C*** BEG ADD *** 11/02/07 ****************************************
     C*
     C* IF THE CHECK AMOUNT AND DATE ARE ZERO,
     C*  USE THE ORIGINAL EOMB PRINT DATE.
     C*
     C     EAPAID        IFEQ      *ZERO
     C     EAWCPD        ANDEQ     *ZERO
     C                   MOVE      EAEPDT        XXWCPD
     C                   ENDIF
     C*
     C* LOAD THE CHECK # AND AMOUNT
     C*
     C                   MOVE      *BLANK        XXCK#
     C                   MOVE      EAWCPC        XXWCPC
     C*
     C* THE FOLLOWING CODE CONVERTS -219.77 TO
     C*   '-00000000000000000000000000219.77'
     C* AND ALSO                     219.77 TO
     C*   ' 00000000000000000000000000219.77'
     C*
     C                   MOVE      *BLANK        XXCKAM
     C                   MOVE      *ZERO         XX32A            32
     C                   MOVE      '.'           XXDECP
     C*
     C                   MOVE      EAPAID        X3PAID
     C                   MOVE      X3CENT        XXCENT
     C                   MOVE      X3AMT         XXAMT
     C                   MOVE      XXPAID        XX32A
     C                   MOVE      XX32A         XXCKAM
     C     EAPAID        IFLT      *ZERO
     C                   MOVEL     '-'           XXCKAM
     C     XXA:XXN       XLATE     XXP96         XXP96
     C                   ENDIF
    C*** END ADD *** 11/02/07 ****************************************
     C                   MOVEL(P)  DATA          N4DATA
     C*
     C                   WRITE     PN401
     C*
     C                   ADD       1             COUNT
     C*
     C                   ENDSR
    C*** BEG ADD *** 02/02/07 ****************************************
     C****************************************************************
     C* GETMDT - Subroutine to retrieve the check mailing date       *
     C****************************************************************
     C*
     C     GETMDT        BEGSR
     C*
     C* USE THE CHECK MAILING DATE IF NOT BLANK
     C*
     C     @@WCIF        IFEQ      'Y'
     C*
     C                   MOVEL     EASUB#        ##SUB#            5
     C                   MOVEL     EAFND         #1FND             3
     C*
     C* DETEMINE IF STANDALONE ACCOUNT
     C*
     C                   MOVE      EACO#         #1CO#             3
     C                   CALL      'FA701R'
     C                   PARM      ' '           ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    ##SUB#
     C                   PARM                    #1FND
     C                   PARM      ' '           ##OUTS            1
     C*
     C     ##OUTS        IFEQ      'N'
     C*
     C* GET WCMS FUND NUMBER
     C*
     C                   MOVE      *BLANKS       ##WCF#            3
     C                   CALL      'FA700R'
     C                   PARM      ' '           ##RTCD
     C                   PARM                    #1CO#
     C                   PARM                    ##WCF#
     C                   PARM                    ##SUB#
     C                   PARM                    #1FND
     C*
     C     EAWCPD        IFGT      *ZERO
     C                   Z-ADD     EAWCPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         XXWCPD            8
     C                   CALL      'WA050R'
     C                   PARM                    #1CO#
     C                   PARM      ##WCF#        #1FND
     C                   PARM      XXWCPD        #1TRDT            8
     C                   PARM      *BLANKS       #1CMDT            8
     C                   ENDIF                                                  EAWCPD>0
     C*
     C                   ENDIF                                                  ##OUTS=N
     C*
     C                   ENDIF                                                  @@WCIF
     C*
     C                   ENDSR
     C****************************************************************
     C* PGMSTR - Retrieve install flags, etc.
     C****************************************************************
     C*
     C     PGMSTR        BEGSR
     C*
     C*  Check to see if WCMS is installed
     C*
     C                   CALL      'SA001R'
     C                   PARM                    @@WCIF            1            WC INST
     C                   PARM                    @@MCIF            1            MC INST
     C                   PARM                    @@WCIN            1            WC INTERFACE
     C*
     C                   ENDSR
    C*** END ADD *** 02/02/07 ****************************************
