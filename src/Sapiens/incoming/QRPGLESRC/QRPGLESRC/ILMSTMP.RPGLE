     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *   10/25/02
      *  Description:                                                   *
      *
      *  This API will View the requested Image Document FOR ANNOTATIONS.
      *  It is called by Delta System software as an api to our View
      *  application
      *  FROM THE RETURNED DOCID/FOLDER, AND NEW DCID/FLR IS CREATED IN THE
      *  SYSTEM USING OUR NORMAL FLR BAL ROUTINES (COPYING THE OPTTEMP VERSI
      *  ON TO THE NEW DCID)
      *
      *  NEW INDEX FOR THE DOC, FROM VAL PASSED IN IN THE IL-LOC-PTH PARM
      *  FROM DELTA.
      *
      *  DELETE (LIKE ILMDELET PGM) THE OLD INDEX)
      *
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
      ** REVISIONS:
      **
      ***  01/08/02 ** CHECKED FOR INVALID KEY FROM IS724R (BLANKS)
      **
      **  11/04/02 ** REMOVED OPTICAL ONLY CHECK ** WILL NOW ALWAYS
      **8             CREATE A NEW DOC, AND INDEX, AND DELETE OLD (SOFT)
      **
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
     FSDCMLE    IF   E           K DISK
     F                                     RENAME(PCM01:PCM0A)

     D EMSG            S             79    DIM(7) CTDATA PERRCD(1)
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
     C     DOKY2         KLIST
     C                   KFLD                    ##TK
     C*
     C     CMKY$         KLIST
     C                   KFLD                    DODKEY
     C                   KFLD                    DODSQ#
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
     C                   MOVEL     QQPATH        ##TOKY           30
     C                   MOVEL     ##TOKY        ##TK             40
     C*
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
      *
      *** GET DO RCD
      *** GET CM RCD
      *** COMPARE ##KEY TO CMKEY (MUST BE EQUAL)
      *** CMIDOC MUST EQUAL INPUT DOCID
      *** CALL SCIMGV WITH PARMS (INC. OPT FLAG)
      ***
     C     DOKY          CHAIN     PDO01                              60
     C     *IN60         IFEQ      *OFF
      * * * GET CM RECORD
     C     CMKY$         CHAIN     PCM0A                              60
     C     *IN60         IFEQ      *OFF
      *
     C     CMKEY         IFNE      ##KEY
      *** ERROR
     C                   MOVEA     EMSG(2)       QQEMSG
     C                   MOVEL     '02'          QQRTCD
     C                   GOTO      TERM
      *
     C                   ENDIF
      *
     C     CMIDOC        IFNE      DODCID
      * * * ERROR DOCIDS DONT MATCH
      *
     C                   MOVEA     EMSG(3)       QQEMSG
     C                   MOVEL     '03'          QQRTCD
     C                   GOTO      TERM
      *
     C                   ENDIF
      *
     C                   ELSE
      * * * ERROR NO CM RECORD
     C                   MOVEA     EMSG(4)       QQEMSG
     C                   MOVEL     '04'          QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   ELSE
      * * *  ERROR - NO DOP RECORD
     C                   MOVEA     EMSG(5)       QQEMSG
     C                   MOVEL     '05'          QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   MOVE      CMSEQ#        XXSEQ             5
      *
     C*
     C                   CALL      'ILMSTMP2'
     C                   PARM      DODCID        XADCID           12
     C                   PARM      CMFOLD        XAFOLD           80
     C                   PARM      CMKEY         XAKEY            40
     C                   PARM      XXSEQ         XASEQ             5
     C                   PARM      CMFL05        XAOPT             1
     C                   PARM      *BLANK        IVRTCD            6
      *
     C*
     C     IVRTCD        IFNE      '     0'
     C     IVRTCD        ANDNE     '000000'
     C     IVRTCD        ANDNE     '      '
     C*
      * * * ERROR
     C                   MOVEA     EMSG(1)       QQEMSG
     C                   MOVEL     '99'          QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C***
     C**** PROCESS NEW DOCUMENT HERE *** IF WAS OPTICAL (CMFL05 = 'O')
     C*******    CMFL05    IFEQ 'O' ****** DEL 11/04/02 *************
     **
     C*** MUST GET NAME FOR NEW DOCUMENT & FOLDER
     C***    (USE FOLDER BALANCE CALL)
     C                   CALL      'IS716R'
     C                   PARM      'Z1'          XBWSID            2
     C                   PARM                    XBSEQ#            6
     C                   PARM                    XBSUFX            3
     C                   PARM                    XBBCHS            4
     C                   PARM                    XBDCID           12
     C                   PARM                    XBBCHI           12
     C                   PARM                    XBCIF             8
     C                   PARM                    XBCISF           12
     C                   PARM                    XBRTCD            1
     C*
     C*** COPY OPTICAL DOCID INTO THE NEW NAMEC
     C     XBCIF         CAT(P)    '/':0         ##TOPT           80
     C                   CAT(P)    XBCISF:0      ##TOPT           80
     C*
     C*
     C                   CALL      'SCPYDOC'
     C                   PARM                    XADCID
     C                   PARM                    XAFOLD
     C                   PARM                    XBDCID
     C                   PARM                    ##TOPT
     C                   PARM      *BLANKS       ##RCD             1
     C*
      *
     C     ##RCD         IFNE      *BLANKS
     C                   MOVEA     EMSG(7)       QQEMSG
     C                   MOVEL     *BLANKS       QQRTCD
     C                   MOVE      ##RCD         QQRTCD
     C                   ENDIF
      *
     C*
     C***
     C*
     C*
     C*** DO INDEXING OF NEW DCID USING THE (TO KEY INFO PROVIDED BY DELTAS
     C***    CALL (USING IL-LOCATION-PATH)
     C*******
     C     DOBYTD        MULT      1000          X#BYTD            9 0
     C                   MOVE      X#BYTD        QQBYTD
     C*
     C                   MOVE      DOSDT         QQSDAT
     C                   MOVE      DOSTMD        QQSTIM
     C                   MOVE      DOPGS         QQPGS
     C*
     C                   CALL      'IS724R'
     C                   PARM                    XBWSID
     C                   PARM                    XBSEQ#
     C                   PARM                    XBBCHS
     C                   PARM                    XBCIF
     C                   PARM                    XBCISF
     C                   PARM      '00001'       QQ#DCS            5
     C                   PARM      DODTYP        QQDCTP            8
     C                   PARM                    XBDCID           12
     C                   PARM                    QQBYTD            9
     C                   PARM                    QQSDAT            8
     C                   PARM                    QQSTIM            6
     C                   PARM      DOBCHI        QQBCHI           12
     C                   PARM                    QQPGS             5
     C                   PARM      ##TOKY        QQNKY            40
     C                   PARM      *ZEROS        QQNSQ#            5 0
     C                   PARM      *BLANK        QQRTC             1
     C***
      *
     C     QQRTC         IFNE      *BLANKS
     C*** ADD 01/08/02 *******************************
     C     QQNKY         OREQ      *BLANKS
     C*** END ADD 01/08/02 ****************************
     C                   MOVEA     EMSG(6)       QQEMSG
     C                   MOVEL     *BLANKS       QQRTCD
     C                   MOVE      QQRTC         QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
      *
     C***
     C*** DELETE (SOFT DELETE OF THE OLD INDEX)
     C***
     C                   CALL      'ILMDLTS'
     C                   PARM                    IMGPRM
     C*
     C***
     C*** GET NEW DOCS VALUES TO PASS BACK TO CALLER (INC. DORCD, CMFOLD,&
     C***     DOCTYPE
     **
     C*****************    ENDIF ********** DEL 11/04/02 ***
     C*
     C*
     C     DOKY2         CHAIN     PDO01                              60
     C*
     C***
     I*** BEGIN CHANGES *** USE DS AS PARM *** 06/20/02 ******************
     C                   MOVEL     DODS          $#DOR1
     C                   MOVE      DODS          $#DOR2
     C                   MOVEL     DODCID        QQDCID
     C                   MOVEL     DODKEY        QQDRKY
     C****** END CHANGES 06/20/02 *** USE DS FOR PARM ******************
     C                   MOVEL     DODS          DORCD
     C*
     C                   MOVEL     DODTYP        QQDCTP
     C                   MOVE      DODSQ#        $RSQ#             5
     C     DOFLR         CAT(P)    '/':0         $RFLR            80
     C                   CAT(P)    DOSFLR:0      $RFLR
     C                   MOVEL     $RFLR         QQPATH
     C*
     C*
     C                   CALL      'SCIMGV'
     C                   PARM      DODCID        XADCID           12
     C                   PARM      QQPATH        XAFOLD           80
     C                   PARM      DODKEY        XAKEY            40
     C                   PARM      $RSQ#         XASEQ             5
     C                   PARM      *BLANK        XAOPT             1
     C*********************PARM *BLANK    IVRTCD  6
     C                   PARM      'ILMSTP'      IVRTCD            6
      *
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
      *
** EMSG
Document information is unavailable. (IS711R) Rtncode-999999
Key value does not match CM record.  Index mismatch. Rtncode-02
Docid mismatch CM Record.  Index Mismatch.  Rtncode-03
No CM record found.  Unable to retrieve values. Rtncode-04
No DO record found.  Unable to retrieve values. Rtncode-05
Can not Create New Document (IS724R). RtnCode -06
Error on SCPYDOC program.  Unable to copy document. Rtncode-07
