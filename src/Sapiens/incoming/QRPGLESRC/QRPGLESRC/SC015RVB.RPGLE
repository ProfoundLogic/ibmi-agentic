     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *===============================================================*
      *===============================================================*
     H/TITLE SC015RVB - Visual DARTS call program
      *===============================================================*
      * Program Description:  This program calls the CL to connect    *
      *   to the PC program for Visual DARTS.                         *
      *                                                               *
      * Called From: SC015R                                           *
      * Calls To   : SC015CVB                                         *
      *                                                               *
      * Created    : 11/19/99                                         *
      * Programmer : C:113                                            *
      * Project    : DR079                                            *
      *===============================================================*
      *  REVISIONS:                                                   *
     H*
     H* 08/03/13  C:33  Implement GS (no comments)
      *                                                               *
      *  10/10/12  C:33  Project: SY266                               *
      *                  Open in a new Atrium tab                     *
      *                                                               *
      * MODIFIED   : 10/25/06                                         *
      * Programmer : C:002                                            *
      * Project    : DR214                                            *
      *
      * ****   IDARTS ADDED AS POSSIBLE CALL.
      *                                                               *
      *                                                               *
      *  09/25/01 C:128 Project: DR152                                *
      *                 If the call to this PGM came from Diary Summ. *
      *                 Set the remark code type QQRMCD to ~~G~~      *
      *                 before going to Visual Darts. This will       *
      *                 make Visual Darts display the user Diary      *
      *                 in Group Summary form.                        *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: ADD NEW COMMENTS TO THE TOP OF REVISIONS.              *
      * ------------------------------------------------------------- *
      *===============================================================*
     F**** ADD 10/25/06 ***********************************************
     FSAIDL     IF   E           K DISK
     F**** END ADD 10/25/06 *******************************************
      *=========================================================================
      *  Data Structure and Constant Definitions
      *=========================================================================

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     DisUseGS          s              1n   inz

     D***  Begin Add  ***  10/10/12  **************************************
     D/copy sPrAtrium

     DstrURL           s            720
     DstrUrlTabName    s             50    inz('iDarts')
     DpgmName          s             10    inz('W0070R')
     D***  End   Add  ***  10/10/12  **************************************
      *=========================================================================
      *  PLIST Definitions
      *=========================================================================
      *
      ** Entry parameters

     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQRMCD            5
     C                   PARM                    QPDATE            8
     C                   PARM                    QQUSER           10
      *
      ** Retrieve the viewer dataqueue parameters
     C     VIEWPL        PLIST
     C                   PARM                    Q1JOB
     C                   PARM      *BLANKS       QQIDRV            1
     C                   PARM      *BLANKS       QQVQUE           10
     C                   PARM      *BLANKS       QQVQLB           10
     C                   PARM      *BLANKS       QQVRQ            10
     C                   PARM      *BLANKS       QQVRQL           10
     C                   PARM      *BLANKS       QQRTCD            1
      *
     C     VDARPL        PLIST
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD
     C                   PARM                    QQRMCD
     C                   PARM                    QPDATE
     C                   PARM                    QQUSER
     C                   PARM                    QQVQUE
     C                   PARM                    QQVQLB
     C                   PARM                    QQVRQ
     C                   PARM                    QQVRQL
     C                   PARM                    Q1USER
     C*** ADD ****  *** 10/25/06 **************************************
     C*
     C     INSTAL        PLIST
     C                   PARM                    @@WCIF            1             1
     C                   PARM                    @@MCIF            1             2
     C                   PARM                    @@WCIN            1             3
     C                   PARM                    @@AR              1             4
     C                   PARM                    @@NTWK            1             5
     C                   PARM                    @@UR              1             6
     C                   PARM                    @@MED             1             7
     C                   PARM                    @@EDI             1             8
     C                   PARM                    @@WCDK            1             9
     C                   PARM                    @@RHAB            1            10
     C                   PARM                    @@MSS             1            11
     C                   PARM                    @@FSCH            1            12
     C                   PARM                    @@PRVB            1            13
     C                   PARM                    @@NPWC            1            14
     C                   PARM                    @@OS              1            15
     C                   PARM                    @@ARLS            1            16
     C                   PARM                    @@CCOD            1            17
     C                   PARM                    @@CRVW            1            18
     C                   PARM                    @@PROM            1            19
     C                   PARM                    @@MCRV            1            20
     C                   PARM                    @@FREQ            1            21
     C                   PARM                    @@CRAC            1            22
     C                   PARM                    @@PREO            1            23
     C                   PARM                    @@PARN            1            24
     C                   PARM                    @@MGRI            1            25
     C                   PARM                    @@KCIF            1            26
     C                   PARM                    @@CURS            1            27
     C                   PARM                    @@PCIB            1            28
     C                   PARM                    @@CLT#            3            29
     C                   PARM                    @@UCPF            1            30
     C                   PARM                    @@DCF             1            31
     C                   PARM                    @@AUTB            1            32
33   C                   PARM                    @@DEIF            1            33 DE/Image Flag
     C                   PARM                    @@AURE            1            34 Auto-Reinstate
35   C                   PARM                    @@CRED            1            35 Credit Module
36   C                   PARM                    @@IDRT            1            36 Idarts Module
     ***
     C     PL033         PLIST
     C                   PARM      *BLANKS       @@RDTQ           10
     C                   PARM      *BLANKS       @@SDTQ           10
     C                   PARM      *BLANKS       @@DTQL           10
     C                   PARM      *ZEROS        @@DT05            7 0
      *
      ** INTERNET DARTS CALL PGM   parameters
     C     IDARPL        PLIST
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD
     C                   PARM                    QQRMCD
     C                   PARM                    QPDATE
     C                   PARM                    QQUSER
     C                   PARM                    QQVQUE
     C                   PARM                    QQVQLB
     C                   PARM                    QQVRQ
     C                   PARM                    QQVRQL
     C                   PARM                    Q1USER
     C                   PARM                    @@RDTQ
     C                   PARM                    @@SDTQ
     C                   PARM                    @@DTQL
     C                   PARM                    @@DT05
     ***
     C*
     C*** END ADD ***** 10/25/06 **************************************
      *=========================================================================
      *  Main
      *=========================================================================
      *
      *** Begin Add *** 09/25/01 ***********************************************
     C                   MOVE      QQRTCD        XXRTCD            1
     C                   MOVE      *BLANK        QQRTCD
      *** End   Add *** 09/25/01 ***********************************************
      *
     C     QQUSER        IFEQ      *BLANKS
     C                   MOVEL     Q1USER        QQUSER
     C                   END
     C*** BEG ADD *** 10/25/06 ***************************************
     C                   CALL      'SA001R'      INSTAL
     C     @@IDRT        IFEQ      'Y'
     C                   CALL      'SA033R'      PL033
     C****  CHECK FOR USER LEVEL OVERRIDE TO IDARTS BEING INSTALLED
     C     Q1USER        CHAIN     PID01                              60
     C     *IN60         IFEQ      *OFF
     C     IDIDRT        ANDEQ     *BLANK
     **       DON'T EXECUTE IDARTS FOR THIS USER
     C                   GOTO      NOID1
     C                   ENDIF
     **
     C     YESID1        TAG
      /If Defined (WEB)
      /Free
       isUseGS = not AtriumJob;

       Select;
       When AtriumJob;
       //***  Begin Add  ***  10/10/12  ***********************************
          strURL = strUrlTabName
           + '/iDartsWeb/profound.do'
           + '?user=' + %trim(qqUser)
           + '&action=listing'
           + '&mode=diary'
           + '&tab=diarySummary';
          Callp LaunchNewTab('Click to open iDarts':'Open iDarts':strURL);
          *inlr = *on;
          Return;
       //***  End   Add  ***  10/10/12  ***********************************
       Endsl;
      /End-free
      /Endif
      *
      *
      ** Viewer dataqueue retrieval program name
     C                   MOVEL     'IS718R1'     XCVIEW           10
      *
      ** INTERNET DARTS CALL PGM   name
     C                   MOVEL     'SC015CID'    XCIDAR           10
      *
      ** Get the viewer dataqueue, as this is the one for DARTS too
     C                   CALL      XCVIEW        VIEWPL
     **
     *** IDARTS IS CALLED REGARDLESS OF THERE BEING A WS RCD OR NOT
     **
     C     XXRTCD        IFEQ      'G'
     C                   MOVE      '~~G~~'       QQRMCD
     C                   ENDIF
     **
     C                   CALL      XCIDAR        IDARPL
      *
     C*
     C                   GOTO      ENDIDR
     **
     C*
     C                   ELSE                                                      ID NOT INSTALLED
     C*
     C****  CHECK FOR USER LEVEL OVERRIDE TO IDARTS BEING INSTALLED
     C     Q1USER        CHAIN     PID01                              60
     C     *IN60         IFEQ      *OFF
     C     IDIDRT        ANDNE     *BLANK
     **       EXECUTE IDARTS REGARDLESS OF THE SYSTEM SETTING
     C                   CALL      'SA033R'      PL033
     C*
     C     @@RDTQ        IFNE      *BLANKS
     C     @@SDTQ        ANDNE     *BLANKS
     C     @@DTQL        ANDNE     *BLANKS
     C                   GOTO      YESID1
     C                   ENDIF                                                  @@RDTQ <> ' '
      *
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C                   ENDIF                                                  @@IDRT="Y"
     C*
     C     NOID1         TAG
     C*
     C*** END ADD *** 10/25/06 ***************************************
      *
      ** Viewer dataqueue retrieval program name
     C                   MOVEL     'IS718R1'     XCVIEW           10
      *
      ** Visual DARTS call program name
     C                   MOVEL     'SC015CVB'    XCVDAR           10
      *
      ** Get the viewer dataqueue, as this is the one for DARTS too
     C                   CALL      XCVIEW        VIEWPL
      *
      ** If RTNCD NOT *BLANK then an error occured...don't do the request
     C     QQRTCD        IFEQ      *BLANKS
      *
      *** Begin Add *** 09/25/01 ***********************************************
     C     XXRTCD        IFEQ      'G'
     C                   MOVE      '~~G~~'       QQRMCD
     C                   ENDIF
      *** End   Add *** 09/25/01 ***********************************************
      *
     C                   CALL      XCVDAR        VDARPL
     C                   ENDIF                                                  QQRTCD = *BLANKS
      *
      ***** ADD 10/25/06 *********************************************
      **
     C     ENDIDR        TAG
      **
      ***** END ADD 10/25/06 *********************************************
     C                   MOVE      *ON           *INLR
