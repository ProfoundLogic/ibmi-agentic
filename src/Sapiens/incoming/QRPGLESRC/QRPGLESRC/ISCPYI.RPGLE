     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *  Description:                                                   *
      *
      *  This API will copy the requested Image Document to an IFS directory
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SEND TO THIS PGM:  Parms
      *
      *  SEND BACK       :
      *  - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
      *                                                                 *
      *  Called From:                                                   *
      *    VARIOUS PROGRAMS                                             *
      *                                                                 *
      *  Calls To:                                                      *
      *    NONE                                                         *
      *                                                                 *
      *  Created:   5/24/02  C:02  Project:                             *
      *                                                                 *
      * *****************************************************************
     H** REVISIONS:
      *
      *   10/17/11 C:279 Project: SY195                               *
      *                  - Modify program to pass/return UNC path     *
      *                    field and IFS flag to called/calling       *
      *                    programs.                                  *
      *                                                               *
      * *****************************************************************
      *  Indicators:                                                    *
      *                                                                 *
      *    60 - General File Access                                     *
      * *****************************************************************
      *
     F*
     FIMDOLB    IF   E           K DISK
      *
     FSDCMLE    IF   E           K DISK
     F                                     RENAME(PCM01:PCM0A)

     D XP              S              1    DIM(60)
     D EMSG            S             79    DIM(25) CTDATA PERRCD(1)
      *
     D QQFILL          DS
     D  XXFIL1                 1    256
     D  XXFIL2               257    340
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D SCCOMM          DS
     D  SCCOM1                 1    256
     D  SCCOM2               257    275
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

     C*
     C*   KEY LIST FOR
     C*
     C     DOKY          KLIST
     C                   KFLD                    QQDCID
     C*
     C     CMKY$         KLIST
     C                   KFLD                    ##KEY
     C                   KFLD                    ##SEQ#
      *
     C***        *LIKE     DEFN XXXX      XXXX
     C*
      * ---------------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID           12            01 IMG DOC
     C                   PARM                    QQDRKY           40            02 DART KEY
     C                   PARM                    QQDRSQ            5            03 DART SQ#
     C                   PARM                    QQPATH           60            04 FROM (RTN)
     C                   PARM                    QQCPTH           60            05 TO PATH
     C                   PARM                    QQEMSG           79            06 EMSG (RTN)
     C                   PARM                    QQRTCD            2            07 RTCD (RTN)
     C*
     C*
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
     C                   MOVEL     QQDRKY        ##KEY            40
     C                   MOVE      QQDRSQ        ##SEQ#            5 0
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
      * * * GET CM RECORD
     C     CMKY$         CHAIN     PCM0A                              60
     C     *IN60         IFEQ      *OFF
      *
     C     DOKY          CHAIN     PDO01                              60
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
     C     CMIMGF        IFEQ      'Y'
     C     CMIMGF        OREQ      'I'
     C     CMIDOC        IFNE      DODCID
      * * * ERROR DOCIDS DONT MATCH
      *
     C                   MOVEA     EMSG(3)       QQEMSG
     C                   MOVEL     '03'          QQRTCD
     C                   GOTO      TERM
      *
     C                   ENDIF
     C                   ELSE
     C     CMDCID        IFNE      DODCID
      * * * ERROR DOCIDS DONT MATCH
      *
     C                   MOVEA     EMSG(3)       QQEMSG
     C                   MOVEL     '03'          QQRTCD
     C                   GOTO      TERM
      *
     C                   ENDIF
     C                   ENDIF
      *
     C                   ELSE
      * * *  ERROR - NO DOP RECORD
      *** IF IMGF IS A "Y", "I", OR "F", THEN ERROR BECAUSE DOP MUST EXIST
      ***   ELSE OTHER OBJECT /DOC... DO ALTERNATE PROCESSING
     C     CMIMGF        IFEQ      'I'
     C     CMIMGF        OREQ      'Y'
     C     CMIMGF        OREQ      'F'
     C*
     C                   MOVEA     EMSG(5)       QQEMSG
     C                   MOVEL     '05'          QQRTCD
     C                   GOTO      TERM
     C                   ELSE
     C*
     C* * * DO ALTERNATE PROCESSING FOR "OTHER" OBJECT TYPE
     C*
     C                   ENDIF
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
     C                   MOVE      CMSEQ#        XXSEQ             5
      *
     C*** build / Determine TOPATH for the copy
     C** if the last character in the string is an "*" then we'll use the
     C** FROM DCID, else use as is:
      *
     C     '*'           SCAN      QQCPTH:1      L                 2 0    90
     C     *IN90         IFEQ      *ON
      *** put dcid to replace the "*" in this path
     C                   MOVEA     QQCPTH        XP(1)
     C                   MOVEA     QQDCID        XP(L)
     C                   MOVEA     XP            XXTPTH
     C                   ELSE
     C                   MOVEL     QQCPTH        XXTPTH
     C                   ENDIF
     C*
     C                   CALL      'ISCPYIC'
     C                   PARM                    QQDCID
     C                   PARM                    CMFOLD
     C                   PARM                    CMKEY
     C                   PARM                    XXSEQ
     C                   PARM                    CMIMGF
     C                   PARM                    CMFL05
     C                   PARM                    XXTPTH           60
     C                   PARM      *BLANK        XXRTCD            2
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   PARM                    CMUNCN
     C                   PARM                    CMIFSP
      ***** END ADD ***** 10/17/11 ***********************************
      *
     C*
     C     XXRTCD        IFNE      *BLANK
     C*
     C     XXRTCD        IFEQ      '50'
     C                   MOVEA     EMSG(24)      QQEMSG
     C                   MOVEL     XXRTCD        QQRTCD
     C                   ELSE
     C*
     C     XXRTCD        IFEQ      '51'
     C                   MOVEA     EMSG(25)      QQEMSG
     C                   MOVEL     XXRTCD        QQRTCD
     C                   ELSE
     C*
     C     XXRTCD        IFEQ      '18'
     C**** NO PROBLEM IF OBJ. ALREADY EXISTS
     C                   ELSE
     C*
     C     XXRTCD        IFEQ      '99'
     C                   MOVEA     EMSG(1)       QQEMSG
     C                   MOVEL     '01'          QQRTCD
     C                   ELSE
     C                   MOVE      XXRTCD        F                 3 0
     C                   ADD       5             F
     C                   MOVEA     EMSG(F)       QQEMSG
     C                   MOVEL     XXRTCD        QQRTCD
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDIF
     C                   MOVEL     CMFOLD        QQPATH
     C*
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
 1 CPFA0AA Error occured while attempting to obtain space
 2 CPFA0AB object name not a directory.
 3 CPFA0A1 An input or output error occured
 4 CPFA0A3 Path name resolution causes looping
 5 CPFA0A6 Number of links exceeds maximum allowed for the file system.
 6 CPFA0A7 Path Name too Long
 7 CPFA0A9 Object not found
 8 CPFA0BB x Objects copied. y Objects failed.
 9 CPFA0B0 Request not allowed to operate from 1 filesystem to another.
10 CPFA0B2 No objects satisfy request.
11 CPFA0C4 Object name not a file.
12 CPFA0DA Object name not a directory.
13 CPFA08E More than one name matches pattern.
14 CPFA085 Home directory not found for user x.
15 CPFA09C Not authorized to Object.
16 CPFA09D Error occured in program x.
17 CPFA093 Name matching pattern not found.
18 CPFA0A0 OBJECT ALREADY EXISTS.
50         IMAGE Values not retrieved (IS711R)
51         OPTICAL program (977) error returned. see low level msgs
