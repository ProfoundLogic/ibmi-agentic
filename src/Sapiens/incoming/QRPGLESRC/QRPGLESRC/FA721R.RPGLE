     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      * *****************************************************************
      /TITLE FA721R - API TO CREATE A TWO LINE HEADING FROM PARMS
      * *****************************************************************
      *  Description:                                                   *
      *
      *  THIS API WILL CREATE A TWO LINE HEADING THAT WILL INCLUDE
      *  CLAIMANT NAME, SOCIAL SECURITY#, DATE OF INJURY,
      *  & CASE# (SUB-GROUP-YEAR-CASE).
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SEND   :  CO#-SUB#-GROUP-YEAR-CASE-LSN-FSN-SS#-DOI
      *  RECEIVE:  TWO 78 CHAR LONG FIELDS FOR HEADING
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  THIS IS WHAT THE HEADING WILL LOOK LIKE:
      *
      *  Claimant name: XXXXXXXXXXXXXXXXXXXXXXXXXXXX
      *  Soc Sec# . . : XXX-XX-XXXX  DOI: XX/XX/XXXX  Case#: XXXXX-XXX-XXX-X(7)X
      *
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *                                                                 *
      *  Called From:                                                   *
      *    VARIOUS PROGRAMS                                             *
      *                                                                 *
      *  Calls To:                                                      *
      *    None                                                         *
      *                                                                 *
      *  Created:  11/30/98  C:94  Project:  SUP15246                   *
      *                                                                 *
      * *****************************************************************
      *  Revisions:                                                     *
      *                                                                 *
      *                                                                 *
      * *****************************************************************
      *
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@LSN            30
     C                   PARM                    @@FSN            15
     C                   PARM                    @@MID             1
     C                   PARM                    @@SS#             9
     C                   PARM                    @@DOI             7 0
     C                   PARM      XLHD1         @@L1             78
     C                   PARM      XLHD2         @@L2             78
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   MOVE      *BLANK        @@RTCD
     C*
      *
      *   LOAD TEXT INTO A FIELD
      *
     C                   MOVEL(P)  'Claimant'    XTCLNM           14
     C                   MOVE      ' name:'      XTCLNM
     C                   MOVEL(P)  'Soc Sec#'    XTSS#            14
     C                   MOVE      ' . . :'      XTSS#
     C                   MOVEL(P)  'DOI:'        XTDOI             4
     C                   MOVEL(P)  'Case#:'      XTCASE            6
      *
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CREATE CLAIMANT NAME STRING
     C     @@LSN         CAT(P)    ',':0         XACLNM           30
     C                   CAT       @@FSN:1       XACLNM
     C     @@MID         IFNE      *BLANK
     C                   CAT       @@MID:1       XACLNM
     C                   CAT       '.':0         XACLNM
     C                   END
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SEPERATE SS#
     C                   MOVE      *ZERO         SS1               3
     C                   MOVE      *ZERO         SSX               6
     C                   MOVE      *ZERO         SS2               2
     C                   MOVE      *ZERO         SS3               4
     C                   MOVEL     @@SS#         SS1
     C                   MOVE      @@SS#         SSX
     C                   MOVEL     SSX           SS2
     C                   MOVE      SSX           SS3
      * CREATE SS# STRING
     C     SS1           CAT(P)    '-':0         XASS#            11
     C                   CAT       SS2:0         XASS#
     C                   CAT       '-':0         XASS#
     C                   CAT       SS3:0         XASS#
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * ACCIDENT DATE
     C                   Z-ADD     @@DOI         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XAACDT            8
      *
      * SEPERATE DATE
     C                   MOVE      *ZERO         XM                2
     C                   MOVE      *ZERO         XNM               2 0
     C                   MOVE      *ZERO         XM1               1
     C                   MOVE      *ZERO         XD                2
     C                   MOVE      *ZERO         XDCY              6
     C                   MOVE      *ZERO         XCY               4
     C                   MOVEL(P)  XAACDT        XM
     C                   MOVEL(P)  XAACDT        XNM
     C                   MOVE(P)   XM            XM1
     C                   MOVE(P)   XAACDT        XDCY
     C                   MOVEL(P)  XDCY          XD
     C                   MOVE(P)   XDCY          XCY
      *
     C     XNM           IFLT      10
     C                   MOVE(P)   *BLANK        XM
     C                   MOVE(P)   XM1           XM
     C                   END
      *
      * CREATE DATE STRING
     C     XM            CAT(P)    '/':0         XBACDT           10
     C                   CAT       XD:0          XBACDT
     C                   CAT       '/':0         XBACDT
     C                   CAT       XCY:0         XBACDT
      *
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SUBSCRIBER#
      *   LOAD SUBSCRIBER # INTO AN ALPHA FIELD
      *
     C                   MOVE(P)   @@SUB#        XASUB#            5
      *
      * LOAD GROUP,YEAR,CASE INTO ALPHA FILEDS
      *
     C                   MOVE(P)   @@FND         XAFND             3
     C                   MOVE(P)   @@FYR         XAFYR             3
     C                   MOVE(P)   @@CASE        XACASE            7
      *
      *  REMOVE LEADING ZEROS FROM SUB#
      *
     C     '0'           CHECK     XASUB#        SP1               3 0          START POSITION
     C     5             SUB       SP1           LN1               3 0
     C                   ADD       1             LN1                            LENGTH
     C     SP1           IFGT      *ZERO
     C     LN1           SUBST(P)  XASUB#:SP1    XASUB#
     C                   END
      *
      *  BUILD CASE# STRING
      *
     C     XASUB#        CAT(P)    '-':0         XASFFC           25
     C                   CAT       XAFND:0       XASFFC
     C                   CAT       '-':0         XASFFC
     C                   CAT       XAFYR:0       XASFFC
     C                   CAT       '-':0         XASFFC
      *
      *  REMOVE LEADING ZEROS FROM CASE
      *
     C     '0'           CHECK     XACASE        SP4               3 0          START POSITION
     C     7             SUB       SP4           LN4               3 0
     C                   ADD       1             LN4                            LENGTH
     C     SP4           IFGT      *ZERO
     C     LN4           SUBST(P)  XACASE:SP4    XACASE
     C                   END
      *
     C                   CAT       XACASE:0      XASFFC
      *
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  All fields have been loaded.  Now create heading lines.
      *  Line 1
     C     XTCLNM        CAT(P)    XACLNM:1      XCCLH            46
     C                   MOVEL(P)  XCCLH         XLHD1            78
      *  Line 2
     C     XTSS#         CAT(P)    XASS#:1       XLHD2            78
     C                   CAT       XTDOI:2       XLHD2
     C                   CAT       XBACDT:1      XLHD2
     C                   CAT       XTCASE:3      XLHD2
     C                   CAT       XASFFC:1      XLHD2
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
