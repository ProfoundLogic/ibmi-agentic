     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8911R1 - WORK FILE BUILD - WHUPP
     H*****************************************************************
     H*                                                               *
     H* DESCRIPTION:                                                  *
     H*                                                               *
     H*   DATA WAREHOUSE - USER PARM FILE - WHUPP
     H*                                                               *
     H* CREATED: 05/16/00  C: 58  WC924                               *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/11/04 C: 132 91DLK                                        *
     H*             Add Exclude Employers file WHXPP, Check file when *
     H*             Write WHUPP record, if exists in WHXPP file then  *
     H*             do not write the record.                          *
     H*                                                               *
     H* 03/25/04 C: 132 CI010                                         *
     H*           - Add start Date PARM for Job Date, so each file    *
     H*             has same ASOF DATE, for next incremetal.  CI010   *
     H*                                                               *
     H* 05/04/01  C:123 PROJECT WC924
     H*                 Add Last Extract Date Input Parm
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
     F*
     F* Group
     FWMUPL     IF   E           K DISK
     F*
      ****** BEG ADD ***** 08/11/04 ************************************
     F*  EXCLUDED EMPLOYERS FILE
     FWHXPL     IF   E           K DISK
      ****** END ADD ***** 08/11/04 ************************************
     F*
     F* Output - Carrier
     FWHUPP     O    E             DISK
     F                                     RENAME(PUP01:PUP02)
     F*

     D*----------------------------------------------------------------
     D*      ARRAYS
     D*----------------------------------------------------------------
     D*
     D*     o Date field arrays
     D*
     D*
     D*    8 and 10 digit dates, numeric and character
     D                 DS
     D  DN                     1      8
     D                                     DIM(8)                               8 digit num
     D  D8X                    1      8
     D  D8N                    1      8  0
     D                 DS
     D  DX                     1     10
     D                                     DIM(10)                              10 dig edited
     D  D10X                   1     10
     D  D10N                   1     10  0
     D*
     D* System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
      *===========================================================
      *  PLIST
      *===========================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@EXDT            8
      ***** BEG ADD ***** 03/25/04 ************************************
     C                   PARM                    @@JBDT            8
     C                   PARM                    @@STYR            3 0
      ***** END ADD ***** 03/25/04 ************************************
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDY'        SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      ****** BEG ADD ***** 08/11/04 ************************************
      *
      *  Employer Excluded file
      *
     C     XPKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    UPEMP#
      ****** END ADD ***** 08/11/04 ************************************
     C*
     C     *LIKE         DEFINE    UPDT          XXEXDT
      *
      *  INITIALIZE Extract Date for Comparison
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVEL     @@EXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     @@EXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
      *
     C                   ELSE
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
      *
     C                   ENDIF                                                  SRERR
      *
      *===========================================================
      *  Begin
      *===========================================================
      *
      *  READ Group file
     C                   READ      PUP01                                  60
     C     *IN60         DOWEQ     '0'
      *
      *   Check Last Extract Date < WMUPL Maint Date, if so, Process
      *
     C     XXEXDT        IFLT      UPDT
      *
      *      As of date
     C                   MOVEL     *BLANKS       D10X
     C                   TIME                    DSDTTM
      *
      ***** BEG CHG ***** 03/25/04 ************************************
     C*************        Z-ADDDSDATE    SRDTI
     C                   MOVEL     @@JBDT        ZZRNDT            8 0
     C                   Z-ADD     ZZRNDT        SRDTI
      ***** END CHG ***** 03/25/04 ************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         D8N
     C                   EXSR      EDDAT
      ***** BEG ADD ***** 03/25/04 ************************************
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         D8N
     C                   EXSR      EDDAT
      ***** END ADD ***** 03/25/04 ************************************
     C                   ENDIF
     C                   MOVE      D10X          UPAODT
      *
      ****** BEG ADD ***** 08/11/04 ************************************
      *  If GET hit on excluded Employer, then bypass the write of record
     C     XPKY          CHAIN     PXP01                              65
     C     *IN65         IFEQ      *ON
      ****** END ADD ***** 08/11/04 ************************************
      *
      *  Write to output file
      *
     C                   WRITE     PUP02
      *
      ****** BEG ADD ***** 08/11/04 ************************************
     C                   ENDIF                                                  END *IN65 *ON
      ****** END ADD ***** 08/11/04 ************************************
      *
     C                   ENDIF                                                  END XXEXDT<UPDT
      *
      *  READ another User Record
      *
     C                   READ      PUP01                                  60
      *
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
     C*================================================================
     C*      EDDAT      EDIT DATE  8 digit to MM/DD/YYYY
     C*================================================================
     C     EDDAT         BEGSR
     C*
     C                   Z-ADD     1             W                 2 0
     C                   DO        10            X                 2 0
     C*
     C     X             IFEQ      3
     C     X             OREQ      6
     C                   MOVEL     '/'           DX(X)
     C                   ELSE
     C                   MOVEL     DN(W)         DX(X)
     C                   ADD       1             W
     C                   ENDIF                                                  .X=3/6
     C*
     C                   ENDDO                                                  .DO 10
     C*
     C                   ENDSR
     C*================================================================
