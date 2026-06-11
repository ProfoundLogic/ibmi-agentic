     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8909R1 - WORK FILE BUILD - WHOFP
     H*****************************************************************
     H*                                                               *
     H* DESCRIPTION:                                                  *
     H*                                                               *
     H*   DATA WAREHOUSE - OFFICE FILE - WHOFP                        *
     H*                                                               *
     H* CREATED: 04/04/00  C:61                                       *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/25/04 C: 132 CI010   and CI015                             *
     H*           - Add start Date PARM for Job Date, so each file    *
     H*             has same ASOF DATE, for next incremetal.  CI010   *
     H*                                                               *
     H* 03/10/04 C: 58  CI015                                         *
     H*             Add start fund year PARM                          *
     H*                                                               *
     H* 02/27/04 C:132 SUPPORT CI099 40909     (ADDED 03/25/04)       *
     H*                Check for certain hex characters in output     *
     H*                alpha fields and replaced if exists. Fields    *
     H*                from tables and static files are not checked   *
     H*                                                               *
     H* 04/24/02  C:132 WCI091 SEQ: 32217                             *
     H*               - Change: Chg Extact date to be > Maint date    *
     H*                                                               *
     H* 07/31/01 C:132 PROJECT WC924                                  *
     H*                ADD Last Extract Date Incoming Parm & Output   *
     H*                if OFFICE File Maint Date > Than Extract Date  *
     H*                                                               *
     H*   C:58 5/15/00  ADD field: FFAODT - Data AS OF date.
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
      *
     F* Office
     FWMOFL     IF   E           K DISK
      *
     F* Output - Office
     FWHOFP     O    E             DISK

     D*
     D*----------------------------------------------------------------
     D*      ARRAYS
     D*----------------------------------------------------------------
     D*
     D*     o Date field arrays
      *
      ***** BEG ADD ***** 02/27/04 ************************************
     D HX              S              1    DIM(60)                              HEX CNVT ARRY
      ***** END ADD ***** 02/27/04 ************************************
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
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
      *===========================================================
      *  PLIST Definitions
      *===========================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@EXDT            8
      ***** BEG ADD ***** 03/25/04 ************************************
     C                   PARM                    @@JBDT            8
      ***** END ADD ***** 03/25/04 ************************************
     C***** BEG ADD ******* 03/10/04 **********************************
     C                   PARM                    @@STYR            3 0
     C***** END ADD ******* 03/10/04 **********************************
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDY'        SRFFMT            8
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
     C*
     C     *LIKE         DEFINE    OFDT          XXEXDT
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
      *  READ OFFICE file
      *
     C                   READ      POF01                                  60
     C     *IN60         DOWEQ     '0'
      *
     C     OFCO#         IFEQ      *ZERO
     C                   GOTO      NEXT
     C                   ENDIF                                                  ALCO# EQ ZERO
      *
      *   Check Last Extract Date against WMOFL Maintenance Date
      *
      **** BEG CHG ***** 04/24/02 **********************************************
     C******     XXEXDT    IFGT OFDT
     C     XXEXDT        IFGE      OFDT
      **** END CHG ***** 04/24/02 **********************************************
     C                   GOTO      NEXT
     C                   ENDIF
      *
      *   Write output record
      *
     C                   EXSR      SROUT
      *
     C     NEXT          TAG
     C                   READ      POF01                                  60
     C                   ENDDO
      *
     C                   MOVE      '1'           *INLR
      *
      *================================================================
      * SRPHN - Format phone number for output
      *================================================================
     C     SRPHN         BEGSR
      *
     C     XXAREA        IFNE      '000'
      *
     C                   MOVEL(P)  '('           XXPHON           20
     C                   CAT(P)    XXAREA:0      XXPHON
     C                   CAT(P)    ')':0         XXPHON
     C                   CAT(P)    XXPH1:1       XXPHON
     C                   CAT(P)    '-':0         XXPHON
     C                   CAT(P)    XXPH2:0       XXPHON
      *
     C                   ELSE
      *
     C                   MOVEL(P)  *BLANKS       XXPHON
     C                   ENDIF
      *
     C                   ENDSR                                                  SRPHN
      *
      *================================================================
      * SROUT - Write record to output file WHOFP
      *================================================================
     C     SROUT         BEGSR
      *
     C                   CLEAR                   PFF01
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     OFNAME        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            OFNAME
      *
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     OFADR1        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            OFADR1
      *
     C     OFADR2        IFNE      *BLANKS
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     OFADR2        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            OFADR2
     C                   ENDIF
      *
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     OFCTY         HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            OFCTY
      *
     C     OFNOIA        IFNE      *BLANKS
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     FFNOIA        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            OFNOIA
     C                   ENDIF
      ***** END ADD ***** 02/27/04 ************************************
      *
     C                   MOVEL(P)  OFOFC#        FFOFC#
     C                   MOVEL(P)  OFNAME        FFNAME
     C                   MOVEL(P)  OFADR1        FFADR1
     C                   MOVEL(P)  OFADR2        FFADR2
     C                   MOVEL(P)  OFCTY         FFCTY
     C                   MOVEL(P)  OFST          FFST
      *
     C                   MOVEL     OFZIP         XXZIP5            5
     C                   MOVE      OFZIP         XXZIP4            4
     C     XXZIP5        CAT(P)    '-':0         FFZIP
     C                   CAT(P)    XXZIP4:0      FFZIP
      *
     C                   MOVE      OFAREA        XXAREA            3
     C                   MOVEL     OFPHN#        XXPH1             3
     C                   MOVE      OFPHN#        XXPH2             4
     C                   EXSR      SRPHN
     C                   MOVEL(P)  XXPHON        FFAPH#
      *
     C                   MOVE      OFFARA        XXAREA            3
     C                   MOVEL     OFFAX#        XXPH1             3
     C                   MOVE      OFFAX#        XXPH2             4
     C                   EXSR      SRPHN
     C                   MOVEL(P)  XXPHON        FFAFX#
      *
     C                   MOVEL(P)  OFNOIA        FFNOIA
      * As Of Date
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
     C                   MOVE      D10X          FFAODT
      *
      *  Write to output file
      *
     C                   WRITE     PFF01
     C                   ENDSR                                                  SROUT
      *
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
     C*
      ***** BEG ADD ***** 02/27/04 ************************************
      *================================================================
      * SRHEX - Check Alpha input field for Invald Hex characters
      *         The Alpha field is loaded into the arrary w/MOVEA
      *         then Loop thru array & replace w/blank if out of
      *         is not a valid HEX Character. After exiting subr
      *         the array is moved back to the alpha field w/MOVEA
      *================================================================
     C     SRHEX         BEGSR
      *
     C                   DO        50            K                 3 0
      *
     C     HX(K)         IFLT      X'40'
     C     HX(K)         OREQ      X'41'
     C     HX(K)         ORGT      X'F9'
     C                   MOVE      X'40'         HX(K)
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   ENDSR                                                  SROUT
      *================================================================
      ***** END ADD ***** 02/27/04 ************************************
