     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     h bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7510R - PRINT CERTIFICATE HOLDERS LETTER
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 11/20/24  C:340 PROJECT: WC2988                               *
     H*                 Add Popup to Indicate this menu is outdated.  *
     H*                                                               *
     H* 07/19/21  C:65  PROJECT: 102WIO/SY371                         *
     H*                 Implement Scriptura output control window     *
     H*                 when process is flagged to generate Scriptura *
     H*                 output.  The settings are stored/passed via   *
     H*                 a local data area which is picked up by       *
     H*                 API's and the printing program.               *
     H*                 Add binding directory above.                  *
     H*                                                               *
     H*  08/30/06 C:61  Support: WA129 - 52370                        *
     H*                 Update xxfnd with change of user paramters.   *
     H*                                                               *
     H*  04/19/06 C:132 Support: WA135 - 52370                        *
     H*               - Blank Copies Parm before loading w/value      *
     H*  04/19/06 C:132 ADD for this project - 02/077/06 C:168 WC1418 *
     H*                 Added 2 new parms to allow the user to print  *
     H*                 control the DARTS remark creation             *
     H*                 - Resolve to DARTS Y/N                        *
     H*                 - DARTS remark code                           *
     H*                                                               *
     H*  07/06/05 C:33  Project: 135WDG                               *
     H*                 Add parm for call of submitter.               *
     H*                                                               *
     H*  08/18/03 C:132 PROJECT: WA099    SEQUENCE: 38403             *
     H*                 ADD F10=SEARCH ON POLICY YEARS.               *
     H*                                                               *
     H*  12/23/98 C:61 PROJECT: WC577                                 *
     H*                Correct call to W0810R.                        *
     H*                                                               *
     H*  12/29/97 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 REPORT STANDARDS.                    *
     H*                DELETED REFERENCES TO SCR99.                   *
     H*                CHANGED DSDATE FORMAT, FOR USE WITH RUN DATE.  *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     FW7510F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*
     F*  USER PARAMETERS MASTER FILE
     F*
     FWMUPL     IF   E           K DISK
     F*
     F*  COMPANY MASTER FILE
     F*
     FSMCOL     IF   E           K DISK
     F*
     F*  GROUP MASTER FILE
     F*
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F*
     F*  GROUP DETAIL FILE
     F*
     FWDF1L     IF   E           K DISK
     F***** BEGIN ADD **** 08/18/03 ***********************************
     F                                     INFDS(DBFDS2)
     F***** END ADD ****** 08/18/03 ***********************************
     F*
     F*  EMPLOYER MASTER FILE
     F*
     FWMEML     IF   E           K DISK
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     F*
     F*  EMPLOYER DETAIL FILE
     F*
     FWDELL1    IF   E           K DISK
     F                                     INFDS(DBFDS1)
     F*
     F*  CERTIFICATE HOLDERS MASTER FILE
     F*
     FWMCEL     IF   E           K DISK
     FWMCEP     IF   E             DISK
     F                                     RENAME(PCE01:PCE02)
      *** begin add ***  07/19/21  ************************************
     FWMEXL     IF   E           K Disk    Rename(pex01:pex00)
     D/copy sPrXML
     d/copy sPrLetter
     D*** Beg Add *** 11/20/24 *** WC2988 ***********************************
      /Copy sPrGenPop
     D*** End Add *** 11/20/24 *** WC2988 ***********************************

     D isUseScript     S              1n   inz
     d processType     c                   const('Certificate of Insurance')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d xxCopies        s              3  0
     d
     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
      *** end add *****  07/19/21  ************************************

     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  KEY LIST FOR THE GROUP MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*  KEY LIST FOR GROUP DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCFYR
     C*
      ****** BEG CHG ***** 08/18/03 **********************************
      * Policy Year Key List
      *
     C     F1KY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR             3 0
      ****** END CHG ***** 08/18/03 **********************************
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*  KEY LIST FOR THE CERTIFICATE HOLDERS FILE
     C*
     C     CEKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCCER#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQCER#            5
     C                   PARM                    QQRTC             1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM      DBRRN1        ##RRN2            9 0
     C                   PARM      2             ##VIEW            1 0
     C*
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      DBRRN         ##RRN2            9 0
     C                   PARM                    ##QTE             1
     C                   PARM      2             ##VIEW            1 0
     C                   PARM                    ##RRN3            9 0
     C*
     C*** Beg Add *** 11/20/24 *** WC2988 *****************************

          PopOneBtn('This menu option is obsolete and should be replaced +
           by new menu option that is available by navigating to +
           to Settings/Control Panel/Navigation.':' ');
          Return;

     C*** End Add *** 11/20/24 *** WC2988 *****************************
     C*
     C                   MOVE      QQFND         XXFND             3 0
     C                   Z-ADD     XXFND         SCFND
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQEMP#        SCEMP#
     C                   MOVE      QQDIV         SCDIV
     C                   MOVE      QQCER#        SCCER#
     C*
     C                   TIME                    DSDTTM
     C*
     C*** BEGIN CHG *** 12/29/97 **************************************
     C**********           MOVE DSDATE    SCRDAT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRDAT
     C*
     C*** END   CHG *** 12/29/97 **************************************
     C*** begin add ***  07/19/21  ************************************
      /Free

       isUseScript = UseScriptura(processType);

      /End-Free
     C*** end add *****  07/19/21  ************************************
     C*
     C                   MOVE      'N'           SCBLNK
     C                   Z-ADD     1             SCCOPY
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C*
     C     QQCO#         IFEQ      *ZERO
     C     *IN60         ANDEQ     *OFF
     C*
     C                   MOVE      UPFND         XXFND             3 0
     C                   Z-ADD     UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR
     C                   END
     C*
     C*
     C     TOP           TAG
     C*
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   MOVE      Q1USER        SCUSER
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*  GET THE GROUP RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   EXFMT     SCR01
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTC
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTC
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 10 = SEARCH LIST CALL ALPHA
     C*
     C     *INKJ         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C     HFLD          IFEQ      'SCCER#'
     C*
     C     ELKEY         CHAIN     PEL01                              99
     C*
      ***** BEG CHG ******* 12/23/98 **********************************
     C***********          CALL 'W0810R'  ACDPL
     C  N99              CALL      'W0810R'      ACDPL
      ***** END CHG ******* 12/23/98 **********************************
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PCE02                              99
     C*
     C                   MOVE      CECER#        SCCER#
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKL EQ *ON
     C                   ENDIF                                                  HFLD
     C*
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   CALL      'W0050R'      ACDPL1
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PEM02                              99
     C*
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKL EQ *ON
     C                   ENDIF                                                  HFLD
     C*
      ****** BEG CHG ***** 08/18/03 **********************************
      * Search on Fund Year
     C     HFLD          IFEQ      'SCFYR'
     C*
     C                   CALL      'W0011R'
     C                   PARM                    ##KC
     C                   PARM                    ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM      DBRRN         ##RRN
     C                   PARM                    XXFYR
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     XXFYR         OREQ      *ZEROS
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL=ON
     C*
     C     XXFYR         IFNE      *ZERO
     C*
     C     F1KY          CHAIN     PF101                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      F1FYR         SCFYR
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF
     C                   GOTO      STRT
     C                   ENDIF
     C                   GOTO      STRT
     C*
      ****** END CHG ***** 08/18/03 **********************************
     C*
     C                   ENDIF                                                  SCR01
     C                   ENDIF                                                  *INKJ
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*   COMMAND 13 - PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      UPFND         SCFND
      ****** beg add ****** 08/30/06 **********************************
     C                   Z-ADD     UPFND         XXFND
      ****** end add ****** 08/30/06 **********************************
     C                   Z-ADD     UPFYR         SCFYR
     C                   Z-ADD     UPEMP#        SCEMP#
     C                   Z-ADD     UPDIV         SCDIV
     C                   GOTO      TOP
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE POLICY YEAR
     C*
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  EMPLOYER AND DIVISION MUST EXIST IN EMPLOYER MASTER
     C*
     C*
     C     SCEMP#        IFNE      0
     C     EMKEY         CHAIN     PEM01                              20
     C*
     C     *IN20         CABEQ     '1'           STRT
     C*
     C*  STATUS CODE MUST BE 'A'-ACTIVE
     C*
     C     EMSTAT        CABEQ     'I'           STRT                     20
     C*
     C*
     C*  THE POLICY YEAR MUST BE VALID FOR THE EMPLOYER
     C*
     C     ELKEY         CHAIN     PEL01                              20
     C*
     C     *IN20         CABEQ     '1'           STRT
     C*
     C                   END
     C*
     C*  CERTIFICATE NUMBER MUST BE VALID IF ENTERED
     C*
     C     SCCER#        IFNE      0
     C     SCEMP#        ANDEQ     0
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C     SCCER#        IFNE      0
     C     CEKEY         CHAIN     PCE01                              24
     C     *IN24         CABEQ     '1'           STRT
     C                   END
     C*
     C*  PRINT BLANK CERTIFICATE FLAG MUST BE "Y" OR "N".
     C*  (IF BLANK CERTFICATE FLAG = "Y" ; EXCLUDE CERTIFICATE HOLDERS
     C*   NAME; ADDRESS AND REMARKS)
     C*
     C     SCBLNK        IFNE      'Y'
     C     SCBLNK        ANDNE     'N'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE RUN DATE
     C*
     C                   Z-ADD     SCRDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRDAT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE NUMBER OF COPIES
     C*
     C     SCCOPY        IFLT      1
     C     SCCOPY        ORGT      99
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*** begin add ***  07/19/21  ************************************
      /Free

       xxSubmit = *on;

       If isUseScript;

          xxProcType = processType;
          If scEmp# <> 0;
             xxMass = ' ';
          Else;
             xxMass = 'Y';
          Endif;

          In *Lock *dtaara;

          Clear svLDA;

          Chain(n) (UPCo#:xxFnd) pex00;
          If %found and EXCICD <> ' ';
             xxDart = 'Y';
             xxRMCD = EXCICD;
          Else;
             xxDart = 'N';
             xxRMCD = ' ';
          Endif;

          xxCopies = SCCopy;
          xx#CopiesA = %editc(xxCopies:'X');

          Out *dtaara;

          xxSubmit = OutputWindow(xxProcType:xxMass);

       Endif;

      /End-Free
     C                   If        xxSubmit = *off
     C                   Goto      Strt
     C                   Endif
     C*** end add *****  07/19/21  ************************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
      *
      ***** BEG ADD ***** 04/19/06 ************************************
      * CLEAR THE QQCOPY BEFORE LOADING IT
     C                   MOVE      *BLANKS       QQCOPY
      ***** END ADD ***** 04/19/06 ************************************
     C*
     C                   MOVE      UPCO#         QQCO#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCEMP#        QQEMP#
     C                   MOVE      SCDIV         QQDIV
     C                   MOVE      SCCER#        QQCER#
     C                   MOVE      SCBLNK        QQBLNK            1
     C                   MOVE      SCRDAT        QQRDAT            8
     C     SCBLNK        IFEQ      'N'
     C                   Z-ADD     1             SCCOPY
     C                   END
     C                   MOVE      SCCOPY        QQCOPY            3
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   CALL      'W7510C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQCER#
     C***  Begin Add  ***  07/06/05  **************************************
     C                   PARM      *ZEROS        QQCERT            5
     C***  End   Add  ***  07/06/05  **************************************
     C                   PARM                    QQBLNK
     C                   PARM                    QQRDAT
     C                   PARM                    QQCOPY
     C                   PARM                    QQOUTQ
      ***** BEG ADD ***** 04/19/06 *** 02/07/06 ************************
     C                   PARM      *BLANK        QQRSFL            1
     C                   PARM      *BLANKS       QQRMCD            5
      ***** END ADD ***** 04/19/06 *** 02/07/06 ************************
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
