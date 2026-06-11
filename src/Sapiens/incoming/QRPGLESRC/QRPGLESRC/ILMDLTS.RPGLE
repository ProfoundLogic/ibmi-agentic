     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *  Description:                                                   *
      *
      *  This API will SOFT delete the Image from the system .
      *  The DO, IE, and SDCMP records are marked DL (or I for CM).
      *  Doc Notes are generated to assure an Audit Trail.
      *  It is called by Delta System software.as an api to our View
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SEND TO THIS PGM:  Parms to initiate scan.
      *
      *  SEND BACK       :  DO record, return code.
      *  - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
      *                                                                 *
      *  Called From:                                                   *
      *    VARIOUS PROGRAMS                                             *
      *                                                                 *
      *  Calls To:                                                      *
      *    NONE                                                         *
      *                                                                 *
      *  Created:   4/24/02  C:02  Project:                             *
      *                                                                 *
      * *****************************************************************
     H** REVISIONS:
      *  06/20/02  CHANGED TO USE A DS AS THE ENTRY PARM 1PARM 1024 LNGTH
      *
      * *****************************************************************
      *  Indicators:                                                    *
      *                                                                 *
      *    60 - General File Access                                     *
      * *****************************************************************
      *
     F*
     FIMDOL     IF   E           K DISK
      *

     D EMSG            S             79    DIM(2) CTDATA PERRCD(1)
      *
     D QQFILL          DS
     D  XXFIL1                 1    256
     D  XXFIL2               257    340
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D DODS          E DS                  EXTNAME(IMDOP)
     D  DORC1                  1    256
     D  DORC2                257    275
     D DORCD           DS
     D  DORC3                  1    256
     D  DORC4                257    275
     D SCCOMM          DS
     D  SCCOM1                 1    256
     D  SCCOM2               257    275
     D                 DS
     D  ##DRKY                 1     30
     D  QQKY1                  1     26
     D  QQSQ                  27     30
     D*** BEGIN CHANGES *** USE DS AS PARM *** 06/20/02 ******************
     D IMGPRM          DS
     D  QQRTCD                 1      2
     D  QQEMSG                 3     81
     D  $#DOR1                82    337
     D  $#DOR2               338    356
     D  QQDRKY               357    386
     D  QQDCID               387    398
     D  QQOUTQ               399    408
     D  QQPATH               409    468
     D  QQCPTH               469    528
     D  QQDCTP               529    536
     D  QQFIL1               537    792
     D  QQFIL2               793   1024
     D****** END CHANGES 06/20/02 *** USE DS FOR PARM ******************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

     C*
     C*   KEY LIST FOR
     C*
     C     DOKY          KLIST
     C                   KFLD                    ##KEY
     C*
      *
     C***        *LIKE     DEFN XXXX      XXXX
     C*
      * ---------------------------------------------------------------
     C     *ENTRY        PLIST
     C*** BEGIN CHANGES *** USE DS AS PARM *** 06/20/02 ******************
     C***                  PARM           QQRTCD  2        01
     C***                  PARM           QQEMSG 79        02
     C***                  PARM           DORCD            03
     C***                  PARM           QQDRKY 30        04
     C***                  PARM           QQDCID 12        05
     C***                  PARM           QQOUTQ 10        06
     C***                  PARM           QQPATH 60        07
     C***                  PARM           QQCPTH 60        08
     C***                  PARM           QQDCTP  8        09
     C***                  PARM           QQFILL           10
     C*
     C                   PARM                    IMGPRM
     C*
     C                   MOVEL     $#DOR1        DORC3
     C                   MOVEL     $#DOR2        DORC4
     C****** END CHANGES 06/20/02 *** USE DS FOR PARM ******************
      *-------------------------------------------------------------------
     C***        Q1PRMS    IFGT 15
      *-------------------------------------------------------------------
     C*
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
     C                   MOVE      QQDRKY        ##DRKY
     C                   MOVEL     QQDRKY        ##KEY            40
      *
      * GET TODAY'S DATE
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              8 0
     C*
     C*
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCCDAT           80
     C*
      *** GET DO RCD
      ***
     C     DOKY          CHAIN     PDO01                              60
     C     *IN60         IFEQ      *OFF
      *
     C**** Call program to mark records as deleted.
     **
     C                   CALL      'ILMDLTS2'
     C                   PARM                    DODCID
     C                   PARM      *BLANK        X@RTNC            1
     C     X@RTNC        IFNE      *BLANK
     C******** ERROR!!
     C                   MOVEA     EMSG(2)       QQEMSG
     C                   MOVEL     '02'          QQRTCD
     C                   ENDIF
     C*
     C************************
      *
     C                   ELSE
      * * *  ERROR - NO DOP RECORD
     C                   MOVEA     EMSG(1)       QQEMSG
     C                   MOVEL     '01'          QQRTCD
     C                   ENDIF
      *
     I*** BEGIN CHANGES *** USE DS AS PARM *** 06/20/02 ******************
     C                   MOVEL     DODS          $#DOR1
     C                   MOVE      DODS          $#DOR2
     C****** END CHANGES 06/20/02 *** USE DS FOR PARM ******************
     C                   MOVEL     DODS          DORCD
     C                   MOVEL     DODTYP        QQDCTP
     C*
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
      *
** EMSG
No DO record found.  Unable to retrieve values. Rtncode-01
Error on Delete of Image Records.               Rtncode-02
