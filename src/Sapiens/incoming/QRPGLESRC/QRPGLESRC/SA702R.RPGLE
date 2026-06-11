     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
      /TITLE SA702R - API TO CREATE A TWO LINE HEADING
      * *****************************************************************
      *  Description:                                                   *
      *
      *  THIS API WILL CREATE A TWO LINE HEADING THAT WILL INCLUDE
      *  CLAIMANT NAME, STATE CLAIM#, SOCIAL SECURITY#, DATE OF INJURY,
      *  & CASE# (GROUP-YEAR-CASE).
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SEND   :  CO#-GROUP-YEAR-CASE
      *  RECEIVE:  TWO 78 CHAR LONG FIELDS FOR HEADING
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  THIS IS WHAT THE HEADING WILL LOOK LIKE:
      *
      *  Claimant name: XXXXXXXXXXXXXXXXXXXXXXXXXXXX   SCL#: XXXX(25)XXXXXXXXXXX
      *  Soc Sec# . . : XXX-XX-XXXX  DOI: XX/XX/XXXX  Case#: XXX-XXX-X(7)X
      *  CST# . . . . : XXXXXXXXXXXXXXXXXXXXXXXXXXXX  RISK#: XXXXXXXXXXXXXXX
      *  IMR: XXXXXXXXXX  RN: XXXXXXXX  Curr Cont: XXXXXXXX  File Loc: XXXXXXXXX
      *
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *                                                                 *
      *  Called From:                                                   *
      *    VARIOUS PROGRAMS                                             *
      *                                                                 *
      *  Calls To:                                                      *
      *    None                                                         *
      *                                                                 *
      *  Created:  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                   *
      *                                                                 *
      * *****************************************************************
     H** REVISIONS:
     H*  12/08/98 C:31  Project: 87MHO
     H*                 V4: Std Heading: Add Risk#
     H*                 1. Add Risk# to line 4
     H*                                                                 *
     H*  11/13/98 C:94 PROJECT: 87MHG                                   *
     H*                MODIFIED CANCELLED MESSAGE TO LOOK LIKE          *
     H*                *** CANCELLED XX/XX/XX ***                       *
     H*                                                                 *
     H*  11/02/98 C:94 PROJECT: 87MFX                                   *
     H*                ADDED FIELD TO LINE 1 IF EMPLOYER IS             *
     H*                CANCELLED AND THE CANCELLED DATE                 *
     H*                                                                 *
     H*  09/01/98 C:62 PROJECT: 87MFX                                   *
     H*                ADDED TWO ADDITIONAL LINES.                      *
     H*                                                                 *
     H* *****************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*    60 - General File Access                                     *
     H* *****************************************************************
     H*
     FWMCML1    IF   E           K DISK
     F*** BEGIN ADD *** 09/01/98 **************************************
     FSMUPL1    IF   E           K DISK
     FWMEML     IF   E           K DISK
     F*** END   ADD *** 09/01/98 **************************************
      *

     D*** BEGIN ADD *** 09/01/98 **************************************
     D NM1             S              1    DIM(80)
     D NM2             S              1    DIM(80)
     D*** END   ADD *** 09/01/98 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** BEGIN ADD *** 09/01/98 **************************************
     D HEAD1           C                   CONST('CST# . . . . :')
     D HEAD2           C                   CONST('Curr Cont:')
     D HEAD3           C                   CONST('File Loc:')
     D HEAD4           C                   CONST('Employer . . :')
     D HEAD5           C                   CONST('Region . . . :')
     D*** BEG CHG *** 11/13/98 ****************************************
     D*** BEG ADD *** 11/02/98 ****************************************
     D HEAD6           C                   CONST('  *** CANCELLED')
     D**********    '  CANCELLED'         C         HEAD6
     D*** END ADD *** 11/02/98 ****************************************
     D*** END CHG *** 11/02/98 ****************************************
     D*
     D                 DS
     D  XLHD6                  1     78
     D  XXHD1                  1      5
     D  XXUSR1                 6     17
     D  XXHD2                 18     21
     D  XXUSR2                22     33
     D  XXHD3                 34     44
     D  XXUSR3                45     56
     D  XXHD4                 57     66
     D  XXUSR4                67     78
     D*** END   ADD *** 09/01/98 **************************************
     D*** BEG CHG *** 11/13/98 ****************************************
     D*** BEG ADD *** 11/02/98 ****************************************
     D*
     D                 DS
     D  XYCADT                 1     14
     D  XY1                    1      2
     D  SLASH1                 3      3
     D  XY2                    4      5
     D  SLASH2                 6      6
     D  XY3                    7     10
     D  ASTER                 11     14

     I*
     I**********                              1  10 XYCADT
     I**********                              1   2 XY1
     I**********                              3   3 SLASH1
     I**********                              4   5 XY2
     I**********                              6   6 SLASH2
     I**********                              7  10 XY3
     I*** END ADD *** 11/02/98 ****************************************
     I*** END CHG *** 11/13/98 ****************************************
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@CO#             3
     C*******************  PARM           @@SUB#  5
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C*** BEGIN CHG *** 09/01/98 **************************************
     C**********           PARM XLHD1     @@L1   78
     C**********           PARM XLHD2     @@L2   78
     C                   PARM                    @@L1             78
     C                   PARM                    @@L2             78
     C*** END   CHG *** 09/01/98 **************************************
     C*** BEGIN ADD *** 09/01/98 **************************************
     C                   PARM                    @@L3             78
     C                   PARM                    @@L4             78
     C                   PARM                    @@L5             78
     C                   PARM                    @@L6             78
     C*** END   ADD *** 09/01/98 **************************************
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
     C*****************    KFLD           KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEGIN ADD *** 09/01/98 **************************************
     C*
     C**  KEY  LIST FOR EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C*************        KFLD           KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C**  KEY  LIST FOR USER PARM  FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    KKSTAF
     C*** END   ADD *** 09/01/98 **************************************
     C*
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C*********  *LIKE     DEFN CMSUB#    KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C*** BEGIN ADD *** 09/01/98 **************************************
     C     *LIKE         DEFINE    UPSTAF        KKSTAF
     C     *LIKE         DEFINE    CMEMP#        KKEMP#
     C     *LIKE         DEFINE    CMDIV         KKDIV
     C     *LIKE         DEFINE    CMFND         KKWCF#
     C*** END   ADD *** 09/01/98 **************************************
     C*
     C                   MOVE      @@CO#         KKCO#
     C******************** MOVE @@SUB#    KKSUB#
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
     C                   MOVEL(P)  'SCL# :'      XTSCL#            6
     C                   MOVEL(P)  'Soc Sec#'    XTSS#            14
     C                   MOVE      ' . . :'      XTSS#
     C                   MOVEL(P)  'DOI:'        XTDOI             4
     C                   MOVEL(P)  'Case#:'      XTCASE            6
     C*** BEGIN ADD *** 12/08/98 **************************************
     C                   MOVEL(P)  'Risk#:'      XTRISK            6
     C*** END ADD *** 12/08/98 ****************************************
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
      * SEPERATE SS#
     C                   MOVE      *ZERO         SS1               3
     C                   MOVE      *ZERO         SSX               6
     C                   MOVE      *ZERO         SS2               2
     C                   MOVE      *ZERO         SS3               4
     C                   MOVEL     CMSS#         SS1
     C                   MOVE      CMSS#         SSX
     C                   MOVEL     SSX           SS2
     C                   MOVE      SSX           SS3
      * CREATE SS# STRING
     C     SS1           CAT(P)    '-':0         XASS#            11
     C                   CAT       SS2:0         XASS#
     C                   CAT       '-':0         XASS#
     C                   CAT       SS3:0         XASS#
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
     C*******************  MOVE KKSUB#    XASUB#  5 P
      *
      * LOAD GROUP,YEAR,CASE INTO ALPHA FILEDS
      *
     C                   MOVE(P)   KKFND         XAFND             3
     C                   MOVE(P)   KKFYR         XAFYR             3
     C                   MOVE(P)   KKCASE        XACASE            7
      *
      *  REMOVE LEADING ZEROS FROM SUB#
      *
     C*********  '0'       CHECKXASUB#    SP1     30       START POSITION
     C***        5         SUB  SP1       LN1     30
     C***                  ADD  1         LN1              LENGTH
     C***        SP1       IFGT *ZERO
     C***        LN1       SUBSTXASUB#:SP1XASUB#    P
     C***                  END
      *
      *  BUILD CASE# STRING
      *
     C*******    XASUB#    CAT  '-':0     XASFFC 25 P
     C                   CAT(P)    XAFND:0       XASFFC           25
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
     C     XTSCL#        CAT(P)    CMACL#:1      XCSCL            32
     C     XTSCL#        CAT(P)    ' ':1         XCSCL            32
     C                   MOVEL(P)  XCCLH         XLHD1            78
     C                   MOVE      XCSCL         XLHD1
      *  Line 2
     C     XTSS#         CAT(P)    XASS#:1       XLHD2            78
     C                   CAT       XTDOI:2       XLHD2
     C                   CAT       XBACDT:1      XLHD2
     C                   CAT       XTCASE:3      XLHD2
     C                   CAT       XASFFC:1      XLHD2
     C*** BEGIN ADD *** 09/01/98 **************************************
     C*
     C*  Line 3
     C*
     C                   Z-ADD     CMEMP#        KKEMP#
     C                   Z-ADD     CMDIV         KKDIV
     C*
     C     EMKY          CHAIN     PEM01                              99
     C*
     C                   CLEAR                   XXEDN
     C                   CLEAR                   NM1
     C                   CLEAR                   NM2
     C                   CLEAR                   XXNUM
     C                   Z-ADD     1             Y                 3 0
     C*
     C                   MOVEL     CMEMP#        XXNUM            80
     C     '0'           CHECK     XXNUM         XXZERO            3 0
     C                   MOVEA     XXNUM         NM1
     C*
     C     XXZERO        DO        9             X                 3 0
     C                   MOVEA     NM1(X)        NM2(Y)
     C                   ADD       1             Y
     C                   ENDDO                                                  XXZERO->X->7
     C*
     C                   CLEAR                   NM1
     C                   CLEAR                   XXNUM
     C                   ADD       2             Y
     C*
     C                   MOVEL     CMDIV         XXNUM            80
     C     '0'           CHECK     XXNUM         XXZERO            3 0
     C                   MOVEA     XXNUM         NM1
     C*
     C     XXZERO        DO        5             X
     C                   MOVEA     NM1(X)        NM2(Y)
     C                   ADD       1             Y
     C                   ENDDO                                                  XXZERO->X->7
     C*
     C     CMDIV         IFNE      *ZERO
     C                   ADD       2             Y
     C                   ENDIF                                                  XXDIV<>*ZERO
     C*
     C                   MOVEA     EMNAME        NM2(Y)
     C*
     C                   MOVEA     NM2           XXEDN            80
     C*
     C     XXEDN         IFNE      *BLANKS
     C*
     C     HEAD4         CAT(P)    XXEDN:1       XLHD3            78
     C*** BEG ADD *** 11/02/98 ***************************************
     C                   CALL      'WA112R'
      * INPUT FROM CALLER
     C                   PARM                    KKCO#                           I
     C                   PARM                    KKFND                           I
     C                   PARM                    KKEMP#                          I
     C                   PARM                    KKDIV                           I
      * RETURN TO CALLER
     C                   PARM                    XXCDT             8             O
     C                   PARM                    XXRTCD            1             I/O
      *
     C     XXRTCD        IFEQ      'Y'
     C*
     C                   MOVE      '/'           SLASH1
     C                   MOVE      '/'           SLASH2
     C*** BEG ADD *** 11/13/98 ***************************************
     C                   MOVE      ' ***'        ASTER
     C*** END ADD *** 11/13/98 ***************************************
     C*
     C                   MOVEL     XXCDT         XY4               4
     C                   MOVE      XXCDT         XY3
     C                   MOVEL     XY4           XY1
     C                   MOVE      XY4           XY2
     C*
     C*** BEG CHG *** 11/13/98 ***************************************
     C     HEAD6         CAT       XYCADT:1      XYHEAD           30
     C********** HEAD6     CAT  XYCADT:1  XYHEAD 22
     C*** END CHG *** 11/13/98 ***************************************
     C                   MOVE      XYHEAD        XLHD3
     C*
     C                   ENDIF
     C*** END ADD *** 11/02/98 ***************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       XLHD3
     C*
     C                   ENDIF
     C*
     C*  Line 4
     C*
     C                   MOVE      CMCO#         #1CO#             3
     C*******************  MOVE CMSUB#    #1SUB#
     C                   MOVE      CMFND         #1FND             3
     C*
     C****************     CALL 'FA700R'
     C***                  PARM *BLANKS   ##RTCD  1
     C***                  PARM           #1CO#   3
     C***                  PARM           #1WCF#  3
     C***                  PARM           #1SUB#  5
     C*****************    PARM           #1FND   3
     C*
     C                   MOVE      #1CO#         ##CO#
     C                   MOVE      #1FND         ##WCF#
     C******************** MOVE #1WCF#    ##WCF#
     C*
     C                   CALL      'WA021R'
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##WCF#            3 0
     C                   PARM      EMEMP#        ##EMP#            9 0
     C                   PARM      EMDIV         ##DIV             5 0
     C                   PARM      *ZEROS        ##RGN#            3 0
     C                   PARM      *BLANKS       ##DESC           30
     C*** BEGIN ADD *** 12/08/98 **************************************
     C                   CALL      'WA113R'
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##WCF#            3 0
     C                   PARM      EMEMP#        ##EMP#            9 0
     C                   PARM      EMDIV         ##DIV             5 0
     C                   PARM      *BLANKS       ##RSK#           15
     C*** END ADD *** 12/08/98 ****************************************
     C*
     C     ##RGN#        IFNE      *ZEROS
     C*
     C                   CLEAR                   NM1
     C                   CLEAR                   NM2
     C                   CLEAR                   XXNUM
     C                   CLEAR                   XXRN
     C                   Z-ADD     1             Y                 3 0
     C*
     C                   MOVEL     ##RGN#        XXNUM            80
     C     '0'           CHECK     XXNUM         XXZERO            3 0
     C                   MOVEA     XXNUM         NM1
     C*
     C     XXZERO        DO        3             X                 3 0
     C                   MOVEA     NM1(X)        NM2(Y)
     C                   ADD       1             Y
     C                   ENDDO                                                  XXZERO->X->7
     C*
     C                   ADD       1             Y
     C                   MOVEA     ##DESC        NM2(Y)
     C*** BEGIN CHG *** 12/08/98 **************************************
     C********             MOVEANM2       XXRN   80
     C                   MOVEA     NM2           XXRN             28
     C     HEAD5         CAT(P)    XXRN:1        XLHD4            78
     C                   ELSE
     C********             MOVEL*BLANKS   XLHD4
     C                   MOVEL(P)  HEAD5         XLHD4
     C*** END CHG *** 12/08/98 ****************************************
     C*
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 12/08/98 **************************************
     C     ##RGN#        IFNE      *ZEROS
     C     ##RSK#        ORNE      *BLANKS
     C     XTRISK        CAT(P)    ##RSK#:1      XLHD4A           32
     C                   MOVE      XLHD4A        XLHD4
     C                   ELSE                                                   RGN# <> 0
     C                   MOVEL     *BLANKS       XLHD4
     C                   ENDIF                                                  RGN# <> 0
     C*** END ADD *** 12/08/98 ****************************************
     C*
     C*  Line 5
     C*
     C     HEAD1         CAT(P)    '    ':1      XLHD5            78
     C*
     C*  Line 6
     C*
     C                   MOVEL(P)  CMCLEX        KKSTAF
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C                   MOVEL(P)  'IMR:'        XXHD1
     C  N99              MOVEL(P)  UPUSR         XXUSR1
     C   99              MOVEL(P)  *BLANKS       XXUSR1
     C*
     C                   MOVEL(P)  CMNRSE        KKSTAF
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C                   MOVEL(P)  'RN:'         XXHD2
     C  N99              MOVEL(P)  UPUSR         XXUSR2
     C   99              MOVEL(P)  *BLANKS       XXUSR2
     C*
     C                   MOVEL(P)  CMUMAN        KKSTAF
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C                   MOVEL(P)  HEAD2         XXHD3
     C  N99              MOVEL(P)  UPUSR         XXUSR3
     C   99              MOVEL(P)  *BLANKS       XXUSR3
     C*
     C                   MOVEL(P)  CMSUPR        KKSTAF
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C                   MOVEL(P)  HEAD3         XXHD4
     C  N99              MOVEL(P)  UPUSR         XXUSR4
     C   99              MOVEL(P)  *BLANKS       XXUSR4
     C*
     C                   SELECT
     C*
     C     Q1PRMS        WHENLE    8
     C*
     C                   MOVEL(P)  XLHD1         @@L1
     C                   MOVEL(P)  XLHD2         @@L2
     C*
     C     Q1PRMS        WHENLE    10
     C     Q1PRMS        ANDGT     8
     C*
     C                   MOVEL(P)  XLHD1         @@L3
     C                   MOVEL(P)  XLHD2         @@L4
     C*
     C     XLHD4         IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XLHD4         @@L2
     C*
     C     XLHD3         IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XLHD3         @@L1
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  *BLANKS       @@L1
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     XLHD3         IFNE      *BLANKS
     C*
     C                   MOVEL(P)  *BLANKS       @@L1
     C                   MOVEL(P)  XLHD3         @@L2
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  *BLANKS       @@L1
     C                   MOVEL(P)  *BLANKS       @@L2
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     Q1PRMS        WHENGT    10
     C*
     C                   MOVEL(P)  XLHD1         @@L3
     C                   MOVEL(P)  XLHD2         @@L4
     C                   MOVEL(P)  XLHD5         @@L5
     C                   MOVEL(P)  XLHD6         @@L6
     C*
     C     XLHD4         IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XLHD4         @@L2
     C*
     C     XLHD3         IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XLHD3         @@L1
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  *BLANKS       @@L1
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     XLHD3         IFNE      *BLANKS
     C*
     C                   MOVEL(P)  *BLANKS       @@L1
     C                   MOVEL(P)  XLHD4         @@L2
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  *BLANKS       @@L1
     C                   MOVEL(P)  *BLANKS       @@L2
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*** END   ADD *** 09/01/98 **************************************
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
