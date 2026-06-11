     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*  PROGRAM SUBROUTINE CR002R1- GET EXTRA INFO
     H*****************************************************************
     H*****************************************************************
     H*  REVISIONS:                                                   *
      *  09/18/13 C:180 Support WA146 82907                           *
      *                 New renewal quote statuses.                   *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
     F*                  (REMOVED RETRN TO CLOSE FILES)
     F*
     FWAQTL7    IF   E           K DISK

     D*
     D* Begin change *** 09/18/13 **************************************
     D*** QTET            S              1    DIM(15) CTDATA PERRCD(1)          QUOTE
     D*** QTED            S             15    DIM(15) ALT(QTET)
     D QTET            S              1    DIM(25) CTDATA PERRCD(1)             QUOTE
     D QTED            S             25    DIM(25) ALT(QTET)
     D* End   change *** 09/18/13 **************************************
     D*
     D*
     D XCUWTR          C                   CONST('Underwriter: ')
     D XCPSRV          C                   CONST('Policy Services: ')
     D XCQSTS          C                   CONST('Status: ')
     D XCAGT           C                   CONST('Agent: ')
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*** ADD 03/03/07 ***************************************************
     ****
     C*
     C     QTKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQFND
     C                   KFLD                    QQEMP#
     C                   KFLD                    QQDIV
     C                   KFLD                    @@QTE             1
     C                   KFLD                    ##AGT#
     C*
     C     QTKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQFND
     C                   KFLD                    QQEMP#
     C                   KFLD                    QQDIV
     C                   KFLD                    @@QTE
     C*
     C*  AGENT/PRODUCER RETRIEVE PROGRAM PARAMETER LIST
     C*
     C     AGTPL         PLIST
     C                   PARM      QQCO#         ##CO#             3 0
     C                   PARM      QQFND         ##FND             3 0
     C                   PARM      QQEMP#        ##EMP#            9 0
     C                   PARM      QQDIV         ##DIV             5 0
     C                   PARM                    ##FYR             3 0
     C                   PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C                   PARM                    ##AREA            3 0
     C                   PARM                    ##PHON            7 0
     C                   PARM                    ##PLAN            1
     C                   PARM                    ##DATE            8 0
     C                   PARM                    ##NWRT            5 2
     C                   PARM                    ##RNRT            5 2
     C                   PARM                    ##@RT#            3 0
     C***** DEL NOT IN CURRENT VERSION ****    PARM           ##PEML 60
     ****
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     ***
     C*
     C* ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQFND             3 0
     C                   PARM                    QQEMP#            9 0
     C                   PARM                    QQDIV             5 0
     C                   PARM                    XZQTE             1
     C                   PARM                    XXQTE            15
     C                   PARM                    SCANAM           60
     C                   PARM                    SCAGT#            5
     **
     C                   MOVEL     *BLANKS       XXQTE
     C                   MOVEL     *BLANKS       SCANAM
     C                   MOVEL     *BLANKS       SCAGT#
     C*
     **
     C                   EXSR      GETAGT
     C*
     C                   EXSR      GETQS
     **
     C*
     C*
     C                   MOVE      '1'           *INLR
     ********
     C*****===============================================================*
     C     GETQS         BEGSR
     ** GET QUOTE STATUS
     ***
     C     XZQTE         IFEQ      'Q'
     C                   MOVEL     'Q'           @@QTE             1
     C                   ELSE
     C                   MOVEL     'W'           @@QTE
     C                   ENDIF
     **
     **
     **
     C                   MOVEL     *BLANKS       XXQTE
     C                   MOVEL     *BLANKS       SCQTTP
     **
     C*  QUOTE STATUS
     C*
     C     QTKY          SETGT     PQT01
     C     QTKY1         READPE    PQT01                                  60
     C*
B01  C     *IN60         IFEQ      *OFF
     C     QTAGT#        ANDEQ     ##AGT#
     C*
     C                   Z-ADD     1             X                 3 0
     C*
     C     QTTYPE        IFEQ      'T'
     C     QTXFER        ANDNE     *BLANKS
     C                   MOVE      QTXFER        QTTYPE
     C                   ENDIF                                                  QTTYPE=T
     C*
      *** Begin add 09/18/13 *****************************************
      ** Convert to new values
      /free
          Select;
             When QTTYPE = 'F';
                  QTTYPE = 'p';
             When QTTYPE = 'G';
                  QTTYPE = 'q';
          Endsl;
      /End-free
      *** End   add 09/18/13 *****************************************
     C     QTTYPE        LOOKUP    QTET(X)                                60
     C   60              MOVEL     QTED(X)       XXQTE            15
     C  N60              MOVEL     *BLANKS       XXQTE
     C                   MOVEL(P)  QTTYPE        SCQTTP            1
     C*
X01  C                   ELSE
     C*
     C                   MOVEL     *BLANKS       XXQTE
     C                   MOVEL     *BLANKS       SCQTTP
     C*
E01  C                   ENDIF                                                  *IN60=*OFF
     C*
     **
     **
     C     ENDQS         ENDSR
     **
     C*===============================================================
     C* GETAGT - SUBROUTINE TO GET AGENT RECORDS
     C*===============================================================
     C*
     C     GETAGT        BEGSR
     C*
     C                   MOVE      *HIVAL        ##FYR
     C                   MOVE      *BLANKS       SCAGT#
     C                   MOVE      *BLANKS       SCANAM
     C                   MOVE      *ZEROS        ##AGT#
     C                   MOVE      *BLANKS       ##ANAM
     C                   MOVEL     'LR    '      ##ANAM                              SET LR ON
     C*                                                         IN WA002R
     C*
     C                   CALL      'WA002R'      AGTPL
     C*
     C                   MOVEL     ##AGT#        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      'WA003R'      CHGPL
     C                   MOVEL     XXNUM         SCAGT#            5
     C*
     C                   MOVEL(P)  ##ANAM        SCANAM           60
     C*
     C                   ENDSR                                                  GETAGT
     C*
** QTE                                                ADDED THIS ARRAY 3/03/07
PPENDING
QQUOTED
WWRITTEN
CCANCELLED
LLOST
DDECLINED
RREINSTATED
BBOUND
MBOUND W/MONEY
TWRITTEN
FRENEWAL PENDING
GRENEWAL QUOTE
USUBMITTED
bRENEWAL BOUND
dRENEWAL DECLINED
iRENEWAL WAITING
lRENEWAL LOST
mRENEWAL BOUND W/MONEY
pRENEWAL PENDING
qRENEWAL QUOTE
sRENEWAL CLOSED
uRENEWAL SUBMITTED
vRENEWAL UNDERWRITER REVIEW
 *** FUTURE USE
 *** FUTURE USE
