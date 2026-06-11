     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
      * *****************************************************************
      /TITLE FA702R - API TO CREATE A TWO LINE HEADING
      * *****************************************************************
      *  Description:                                                   *
      *
      *  THIS API WILL CREATE A TWO LINE HEADING THAT WILL INCLUDE
      *  CLAIMANT NAME, STATE CLAIM#, SOCIAL SECURITY#, DATE OF INJURY,
      *  & CASE# (SUB-GROUP-YEAR-CASE).
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SEND   :  CO#-SUB#-GROUP-YEAR-CASE
      *  RECEIVE:  TWO 78 CHAR LONG FIELDS FOR HEADING
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  THIS IS WHAT THE HEADING WILL LOOK LIKE:
      *
      *  Claimant name: XXXXXXXXXXXXXXXXXXXXXXXXXXXX   SCL#: XXXX(25)XXXXXXXXXXX
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
      *  Created:  12/29/97  C:47  Project:  87MCB                      *
      *                                                                 *
      * *****************************************************************
      *  Revisions:                                                     *
      *                                                                 *
      *  06/09/20  C:212 Project : SY420                                *
      *                  Mask SSN                                       *
      *                                                                 *
      *  12/12/06 C:62 PROJECT 144MAC                                   *
      *                CONDTION SO THAT CS# IS DISPLAYED RATHER THAN    *
      *                THE STATE CLAIM#                                 *
      *                                                                 *
      * *****************************************************************
      *  Indicators:                                                    *
      *                                                                 *
      *    60 - General File Access                                     *
      * *****************************************************************
      *
     FFMCML1    IF   E           K DISK
      *

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      /copy sPrSecure
      ***** End Add *** 06/09/20 *******************************************

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     d***** Beg Add *** 06/09/20 *******************************************
     d xass#           s             11    inz
     d***** End Add *** 06/09/20 *******************************************
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
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
     C**  KEY  LIST FOR CASE MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C*
     C*** BEGIN ADD *** 12/12/06 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C*
     C* IS THIS TML?
     C*
     C                   MOVE      *OFF          C144              1
     C     ##CLT#        IFEQ      '144'
     C                   MOVE      *ON           C144
     C                   ENDIF
     C*** END   ADD *** 12/12/06 **************************************
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@SUB#        KKSUB#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
     C                   MOVE      '1'           @@RTCD
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *BLANK        @@RTCD
      *
      *   LOAD TEXT INTO A FIELD
      *
     C                   MOVEL(P)  'Claimant'    XTCLNM           14
     C                   MOVE      ' name:'      XTCLNM
     C*** BEGIN ADD *** 12/12/06 **************************************
     C     C144          IFEQ      *OFF
     C*** END   ADD *** 12/12/06 **************************************
     C                   MOVEL(P)  'SCL# :'      XTSCL#            6
     C*** BEGIN ADD *** 12/12/06 **************************************
     C                   ELSE
     C                   MOVEL(P)  'CS # :'      XTSCL#            6
     C                   ENDIF
     C*** END   ADD *** 12/12/06 **************************************
     C                   MOVEL(P)  'Soc Sec#'    XTSS#            14
      /If Not Defined (WEB)
     C                   MOVE      ' . . :'      XTSS#
      /Endif
      /If Defined (WEB)
     C                   MOVE      '     :'      XTSS#
      /Endif
     C                   MOVEL(P)  'DOI:'        XTDOI             4
     C                   MOVEL(P)  'Case#:'      XTCASE            6
      *
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CREATE CLAIMANT NAME STRING
     C     CMLSN         CAT(P)    ',':0         XACLNM           30
     C                   CAT       CMFSN:1       XACLNM
     C     CMMID         IFNE      *BLANK
     C                   CAT       CMMID:1       XACLNM
     C                   CAT       '.':0         XACLNM
     C                   END
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ***** Beg Chg *** 06/09/20 *******************************************
      * SEPERATE SS#
     C***                MOVE      *ZERO         SS1               3
     C***                MOVE      *ZERO         SSX               6
     C***                MOVE      *ZERO         SS2               2
     C***                MOVE      *ZERO         SS3               4
     C***                MOVEL     CMSS#         SS1
     C***                MOVE      CMSS#         SSX
     C***                MOVEL     SSX           SS2
     C***                MOVE      SSX           SS3
      * CREATE SS# STRING
     C***  SS1           CAT(P)    '-':0         XASS#            11
     C***                CAT       SS2:0         XASS#
     C***                CAT       '-':0         XASS#
     C***                CAT       SS3:0         XASS#
      /Free
       xaSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * ACCIDENT DATE
     C                   Z-ADD     CMACDT        SRDTI
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
     C                   MOVE(P)   KKSUB#        XASUB#            5
      *
      * LOAD GROUP,YEAR,CASE INTO ALPHA FILEDS
      *
     C                   MOVE(P)   KKFND         XAFND             3
     C                   MOVE(P)   KKFYR         XAFYR             3
     C                   MOVE(P)   KKCASE        XACASE            7
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
     C                   END                                                    *IN60
      *
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  All fields have been loaded.  Now create heading lines.
      *  Line 1
     C     XTCLNM        CAT(P)    XACLNM:1      XCCLH            46
     C*** BEGIN ADD *** 12/12/06 **************************************
     C     C144          IFEQ      *OFF
     C*** END   ADD *** 12/12/06 **************************************
     C     XTSCL#        CAT(P)    CMSCL#:1      XCSCL            32
     C*** BEGIN ADD *** 12/12/06 **************************************
     C                   ELSE
     C     XTSCL#        CAT(P)    CMSRC:1       XCSCL            32
     C                   ENDIF
     C*** END   ADD *** 12/12/06 **************************************
     C                   MOVEL(P)  XCCLH         XLHD1            78
     C                   MOVE      XCSCL         XLHD1
      *  Line 2
     C     XTSS#         CAT(P)    XASS#:1       XLHD2            78
     C                   CAT       XTDOI:2       XLHD2
     C                   CAT       XBACDT:1      XLHD2
     C                   CAT       XTCASE:3      XLHD2
     C                   CAT       XASFFC:1      XLHD2
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
