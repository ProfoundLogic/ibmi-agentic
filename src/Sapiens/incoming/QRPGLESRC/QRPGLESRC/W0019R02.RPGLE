     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
     H****************************************************************
     H/TITLE W0019R02 - CANCELLATION/REINSTATEMENT MAINTENANCE DATA
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE CANCEL/REIN FILE.        *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0019R - CANCELLATION/REINSTATEMENT MAINTENANCE            *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 06/06/97  PROGRAMMER: C:65  PROJECT: WC704          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  03/25/19  C:180 SUPPORT 78137                                *
     H*                  Bypass folder edit if Scriptura is ON.       *
     H*                                                               *
     H*  09/05/18  C:291 PROJECT: WC2602                              *
     H*                  Add Checkbox Generate state CX transaction on*
     H*                  state removal.                               *
     H*                                                               *
     H*  12/01/16  C:65  PROJECT: SY371                               *
     H*                  Disable the document fields if Scriptura     *
     H*                  has been enabled for cx/re.  Update          *
     H*                  activation group to *caller as well.         *
     H*                                                               *
     H*  06/21/12  C:282 PROJECT: WC2086                              *
     H*                  Modify to load the new screen field          *
     H*                  Require Reinstatement Reason Code.           *
     H*                                                               *
     H*  10/11/06  C:33  Seq#: WA099/41801                            *
     H*                  Use SC305R instead of SC580R.                *
     H*                                                               *
     H* 09/02/05  C:61  PROJECT: WC1359 129WLT                        *
     H*                - Add folder/document prompts for:             *
     H*                   CX cover letter                             *
     H*                   CX confirm cover letter                     *
     H*                   Client specific CX Certificate Letter       *
     H*                   Client specific Reinstatement Cert Letter   *
     H*                                                               *
     H* 07/25/05  C:61  PROJECT: 133WBV                               *
     H*                 Added second screen for CX letters info.      *
     H*                                                               *
     H* 11/10/98  C:87   PROJECT: WC852                               *
     H*                  Remove 'Additional mailing days' and 'Rein-  *
     H*                  statement fee' from screen and removed the   *
     H*                  appropriate processing.                      *
     H*                                                               *
     H* 09/13/98  C:35   PROJECT: WC839                               *
     H*                  ADDED FIELDS FOR STATE CANCELLATION FORM     *
     H*                  FOLDER AND DOCUMENT AND STATE REINSTATEMENT  *
     H*                  FORM FOLDER AND DOCUMENT.                    *
     H*                                                               *
     H*  12/29/97  C:87  PROJECT: WC752                               *
     H*                 -'ACORD DEFAULT REMARK1' ADDED TO SCREEN AND  *
     H*                  WRITTEN TO FILE WTXCP                        *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS USED:                                              *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD                                                     *
     H* N40 - UPDATE/INQUIRY                                          *
     H*  92 - GENERAL F KEY PRESSED                                   *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  30 - (WCR0504) The state code was not found in list.         *
     H*  31 - (WC30001) Record already exists.                        *
     H*  32 - (WCR0500) Invalid date.                                 *
     H*  33 - (WCR0502) CANNOT BE LESS THAN OR EQUAL TO ZERO.         *
     H*  34 - (WCR0547) CANNOT BE LESS THAN ZERO.                     *
     H*  35 - (WCR0547) CANNOT BE LESS THAN ZERO.                     *
     H*                                                               *
     H* SCR02:                                                        *
     H*                                                               *
     H* SCR03:                                                        *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H*****************************************************************
     FW0019F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWTXCP     UF A E             DISK
     F                                     RENAME(PXC01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWTXCL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     FWMFNL     IF   E           K DISK
     FSTSTL3    IF   E           K DISK
      *
      * DARTS Code
     FSTCTL1    IF   E           K DISK
      *
      * NCCI Code
     FWTGTL     IF   E           K DISK
     D*
     D*
      ***** Beg Del ***** 06/21/12 ***************************************
     D*****/COPY SCOPYMT01I
      ***** End Del ***** 06/21/12 ***************************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D OPTNM           C                   CONST('Cx/Rein State Def-
     D                                     inition')
     D #OFSCR          C                   CONST(3)
     D COMAPI          C                   CONST('WA001R')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *** begin add ***  12/01/16  ************************************
     d processTypeC    c                   const('Cancellations')
     d processTypeR    c                   const('Reinstatements')
     D/copy sPrXML
      *** end add *****  12/01/16  ************************************
     C*
      ***** Beg Add ***** 06/21/12 ***************************************
     C/COPY SCOPYMT05C
      ***** End Add ***** 06/21/12 ***************************************
      *
      *
      *================================================================
      *  *LIKE DEFN
      *================================================================
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    XCFND         XXFND
     C     *LIKE         DEFINE    XCCO#         XXCO#
     C     *LIKE         DEFINE    XCST          XXST
     C     *LIKE         DEFINE    XCEFDT        XXEFDT
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *
      *
      *================================================================
      * KLIST
      *================================================================
      *
      *  NCCI Code
      *
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
      *
     C*
     C*  KEY LIST FOR THE SYSTEM-WIDE STATE CODE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY            10
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  CANCELLATION DEFINITION FILE
     C*
     C     XCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C*
      *******Beg Del********06/21/12**********************************
     C*****/COPY SCOPYMT01C
      *******End Del********06/21/12**********************************
     C*
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*  CHECK IF OTHER FUNCTION KEYS HAVE BEEN PRESSED
     C*
     C                   EXSR      OTHFKS
     C*
     C*  IF ANY OTHER FUNCTION KEYS HAVE BEEN PROCESSED, DO NOT
     C*  CHECK FOR ERRORS
     C*
     C     *IN92         IFEQ      *OFF
     C*
     C*  VERIFY STATE AND EFFECTIVE DATE IF IN ADD MODE.
     C*
     C     *IN40         IFEQ      *ON
     C*
     C*  STATE MUST BE VALID.
     C*
     C                   MOVEL     SCST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        9030
     C*
     C*  EFFECTIVE DATE MUST BE VALID
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
B02  C  N90SRERR         IFEQ      '1'
     C                   SETON                                        9032
E02  C                   ENDIF                                                  SRERR=1
     C*
     C*  RECORD MUST NOT ALREADY EXISTS FOR ST/EFDT
     C*
     C                   MOVE      SCST          XXST
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C     XCKY          CHAIN     PXC01                              60
     C  N90*IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN31
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN60 EQ 0
     C*
     C                   ENDIF                                                  *IN40 EQ 1
     C*
     C*  CANCELLATION NOTICE DAYS IS REQUIRED.
     C*
     C  N90SCNOTC        IFLE      *ZERO
     C                   MOVE      *ON           *IN33
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCNOTC IFLE
     C*
     C** ADDITIONAL DAYS CANNOT BE LESS THAN ZERO.
     C** REINSTATEMENT FEE CANNOT BE LESS THAN ZERO.
     C*
      *
      * IF ENTERED, STATE CX FORM FOLDER AND DOCUMENT MUST BE VALID
      *
      *
      **** Begin Add *** 03/25/19 *************************************
     C     isScriptCx    IFEQ      *OFF
      **** End   Add *** 03/25/19 *************************************
     C  N90SCCXFL        IFNE      *BLANKS
      *
     C                   MOVE      ' '           QQRTCD            1
      *
     C                   CALL      'SVALFLR'
     C                   PARM                    SCCXFL
     C                   PARM                    QQRTCD
      *
      *    FOLDER MUST EXIST
      *
     C     QQRTCD        IFEQ      '1'
     C                   SETON                                        90  36
     C                   ENDIF                                                  QQRTCD IFEQ '1'
      *
      *    USER MUST HAVE AUTHORITY TO FOLDER
      *
     C     QQRTCD        IFEQ      '2'
     C                   SETON                                        90  37
     C                   ENDIF                                                  QQRTCD IFEQ '2'
      *
     C                   ELSE
      *
      * FOLDER CANNOT BE BLANK IF DOCUMENT IS ENTERED
      *
     C     SCCXDC        IFNE      *BLANKS
     C                   SETON                                        90  36
     C                   ENDIF                                                  SCCXDC IFNE *BLANKS
      *
     C                   ENDIF                                                  SCCXFL IFNE *BLANKS
      *
      *  DOCUMENT MUST BE VALID (IF ENTERED)
      *
     C  N90SCCXDC        IFNE      *BLANKS
      *
     C                   MOVEL     SCCXDC        QQDCID
     C                   MOVEL     SCCXFL        QQFOLD
      *
     C                   CALL      'SVALDOC'
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM                    QQRTCD
      *
     C     QQRTCD        IFEQ      '1'
     C                   SETON                                        90  38
     C                   ENDIF                                                  QQRTCD IFEQ '1'
      *
      * DOCUMENT CANNOT BE BLANK IF FOLDER IS ENTERED
      *
     C                   ELSE
      *
     C     SCCXFL        IFNE      *BLANKS
     C                   SETON                                        90  38
     C                   ENDIF                                                  SCCXFL IFNE *BLANKS
      *
     C                   ENDIF                                                  SCCXDC IFNE *BLANKS
      **** Begin Add *** 03/25/19 *************************************
     C                   ENDIF
      *
     C     isScriptRE    IFEQ      *OFF
      **** End   Add *** 03/25/19 *************************************
      *
      * IF ENTERED, STATE REIN FORM FOLDER AND DOCUMENT MUST BE VALID
      *
     C  N90SCREFL        IFNE      *BLANKS
      *
     C                   MOVE      ' '           QQRTCD            1
      *
     C                   CALL      'SVALFLR'
     C                   PARM                    SCREFL
     C                   PARM                    QQRTCD
      *
      *    FOLDER MUST EXIST
      *
     C     QQRTCD        IFEQ      '1'
     C                   SETON                                        90  39
     C                   ENDIF                                                  QQRTCD IFEQ '1'
      *
      *    USER MUST HAVE AUTHORITY TO FOLDER
      *
     C     QQRTCD        IFEQ      '2'
     C                   SETON                                        90  41
     C                   ENDIF                                                  QQRTCD IFEQ '2'
      *
     C                   ELSE
      *
      * FOLDER CANNOT BE BLANK IF DOCUMENT IS ENTERED
      *
     C     SCREDC        IFNE      *BLANKS
     C                   SETON                                        90  39
     C                   ENDIF                                                  SCREDC IFNE *BLANKS
      *
     C                   ENDIF                                                  SCFOLD<>*BLANKS
      *
      *  DOCUMENT MUST BE VALID (IF ENTERED)
      *
     C  N90SCREDC        IFNE      *BLANKS
      *
     C                   MOVEL     SCREDC        QQDCID
     C                   MOVEL     SCREFL        QQFOLD
      *
     C                   CALL      'SVALDOC'
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM                    QQRTCD
      *
     C     QQRTCD        IFEQ      '1'
     C                   SETON                                        90  42
     C                   ENDIF                                                  QQRTCD IFEQ '1'
      *
      * DOCUMENT CANNOT BE BLANK IF FOLDER IS ENTERED
      *
     C                   ELSE
      *
     C     SCREFL        IFNE      *BLANKS
     C                   SETON                                        90  42
     C                   ENDIF                                                  SCREFL IFNE *BLANKS
      *
     C                   ENDIF                                                  SCREDC IFNE *BLANKS
      **** Begin Add *** 03/25/19 *************************************
     C                   ENDIF
      **** End   Add *** 03/25/19 *************************************
      *
      **** BEGIN ADD **** 06/21/12 ************************************
      /Free
       // REQUIRE REINSTATEMENT REASON CODE MUST BE 'Y' OR BLANKS
        If scRrCd <> *blanks AND scRrCd <> 'Y' ;
           *IN90 = *On ;
           *IN43 = *On ;
        EndIf ;                                // SCRRRC VALIDATION
      /End-Free
      **** END   ADD **** 06/21/12 ************************************
     C                   ENDIF                                                  *IN92=*OFF
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
      *
      *  Number of Months
      *
     C  N90SCCXMN        IFEQ      *ZERO
     C     SCCXLM        ANDNE     *ZERO
     C                   SETON                                        90  71
     C                   ENDIF
      *
      *  1st CX
      *
      *    If Folder or Document entered
      *
     C  N90SC#1LF        IFNE      *BLANKS
     C     SC#1LD        ORNE      *BLANKS
      ***
      *
      *    Folder and Document mutually inclusive
      *
     C  N90SC#1LF        IFNE      *BLANKS
     C     SC#1LD        ANDEQ     *BLANKS
     C     SC#1LF        OREQ      *BLANKS
     C     SC#1LD        ANDNE     *BLANKS
     C                   SETON                                        90  72
     C                   ENDIF
      ***
      *
      *    Folder must be valid
      *
     C  N90SC#1LF        IFNE      *BLANKS
     C                   MOVEL(P)  SC#1LF        ##FLR
      *
     C                   CALL      'SVALFLR'
     C                   PARM                    ##FLR            80
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RTCD        IFNE      *BLANK
     C                   SETON                                        90  73
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  LF valid
      ***
      *
      *    Document must be valid
      *
     C  N90SC#1LD        IFNE      *BLANKS
     C                   MOVEL(P)  SC#1LD        ##DCID
     C                   MOVEL(P)  SC#1LF        ##FLR
      *
     C                   CALL      'SVALDOC'
     C                   PARM                    ##DCID           12
     C                   PARM                    ##FLR            80
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RTCD        IFEQ      *ON
     C                   SETON                                        90  74
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  LD
      ***
     C                   ENDIF                                                  LF + LD
      *
      *  DARTS code must be valid, if entered
      *
     C  N90SC#1DR        IFNE      *BLANKS
     C     SC#1DR        CHAIN     PCT01                              60
      *
     C     *IN60         IFEQ      *ON
     C     CTSTAT        OREQ      'I'
     C                   SETON                                        90  75
     C                   ENDIF                                                  DART Code
      *
     C                   ENDIF                                                  SC#1DR
      ********
      ********
      *
      *  2nd CX
      *
      *    If Folder or Document entered
      *
     C  N90SC#2LF        IFNE      *BLANKS
     C     SC#2LD        ORNE      *BLANKS
      ***
      *
      *    Folder and Document mutually inclusive
      *
     C  N90SC#2LF        IFNE      *BLANKS
     C     SC#2LD        ANDEQ     *BLANKS
     C     SC#2LF        OREQ      *BLANKS
     C     SC#2LD        ANDNE     *BLANKS
     C                   SETON                                        90  82
     C                   ENDIF
      ***
      *
      *    Folder must be valid
      *
     C  N90SC#2LF        IFNE      *BLANKS
     C                   MOVEL(P)  SC#2LF        ##FLR
      *
     C                   CALL      'SVALFLR'
     C                   PARM                    ##FLR            80
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RTCD        IFNE      *BLANK
     C                   SETON                                        90  83
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  LF valid
      ***
      *
      *    Document must be valid
      *
     C  N90SC#2LD        IFNE      *BLANKS
     C                   MOVEL(P)  SC#2LD        ##DCID
     C                   MOVEL(P)  SC#2LF        ##FLR
      *
     C                   CALL      'SVALDOC'
     C                   PARM                    ##DCID           12
     C                   PARM                    ##FLR            80
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RTCD        IFEQ      *ON
     C                   SETON                                        90  84
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  LD
      ***
     C                   ENDIF                                                  LF + LD
      *
      *  DARTS code must be valid, if entered
      *
     C  N90SC#2DR        IFNE      *BLANKS
     C     SC#2DR        CHAIN     PCT01                              60
      *
     C     *IN60         IFEQ      *ON
     C     CTSTAT        OREQ      'I'
     C                   SETON                                        90  85
     C                   ENDIF                                                  DART Code
      *
     C                   ENDIF                                                  SC#2DR
      ********
      ********
      *
      *  3rd CX
      *
      *    If Folder or Document entered
      *
     C  N90SC#3LF        IFNE      *BLANKS
     C     SC#3LD        ORNE      *BLANKS
      ***
      *
      *    Folder and Document mutually inclusive
      *
     C  N90SC#3LF        IFNE      *BLANKS
     C     SC#3LD        ANDEQ     *BLANKS
     C     SC#3LF        OREQ      *BLANKS
     C     SC#3LD        ANDNE     *BLANKS
     C                   SETON                                        90  22
     C                   ENDIF
      ***
      *
      *    Folder must be valid
      *
     C  N90SC#3LF        IFNE      *BLANKS
     C                   MOVEL(P)  SC#3LF        ##FLR
      *
     C                   CALL      'SVALFLR'
     C                   PARM                    ##FLR            80
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RTCD        IFNE      *BLANK
     C                   SETON                                        90  23
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  LF valid
      ***
      *
      *    Document must be valid
      *
     C  N90SC#3LD        IFNE      *BLANKS
     C                   MOVEL(P)  SC#3LD        ##DCID
     C                   MOVEL(P)  SC#3LF        ##FLR
      *
     C                   CALL      'SVALDOC'
     C                   PARM                    ##DCID           12
     C                   PARM                    ##FLR            80
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RTCD        IFEQ      *ON
     C                   SETON                                        90  24
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  LD
      ***
     C                   ENDIF                                                  LF + LD
      *
      *  DARTS code must be valid, if entered
      *
     C  N90SC#3DR        IFNE      *BLANKS
     C     SC#3DR        CHAIN     PCT01                              60
      *
     C     *IN60         IFEQ      *ON
     C     CTSTAT        OREQ      'I'
     C                   SETON                                        90  25
     C                   ENDIF                                                  DART Code
      *
     C                   ENDIF                                                  SC#3DR
      ********
      ********
      *
      *  4th CX
      *
      *    If Folder or Document entered
      *
     C  N90SC#4LF        IFNE      *BLANKS
     C     SC#4LD        ORNE      *BLANKS
      ***
      *
      *    Folder and Document mutually inclusive
      *
     C  N90SC#4LF        IFNE      *BLANKS
     C     SC#4LD        ANDEQ     *BLANKS
     C     SC#4LF        OREQ      *BLANKS
     C     SC#4LD        ANDNE     *BLANKS
     C                   SETON                                        90  32
     C                   ENDIF
      ***
      *
      *    Folder must be valid
      *
     C  N90SC#4LF        IFNE      *BLANKS
     C                   MOVEL(P)  SC#4LF        ##FLR
      *
     C                   CALL      'SVALFLR'
     C                   PARM                    ##FLR            80
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RTCD        IFNE      *BLANK
     C                   SETON                                        90  33
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  LF valid
      ***
      *
      *    Document must be valid
      *
     C  N90SC#4LD        IFNE      *BLANKS
     C                   MOVEL(P)  SC#4LD        ##DCID
     C                   MOVEL(P)  SC#4LF        ##FLR
      *
     C                   CALL      'SVALDOC'
     C                   PARM                    ##DCID           12
     C                   PARM                    ##FLR            80
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RTCD        IFEQ      *ON
     C                   SETON                                        90  34
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  LD
      ***
     C                   ENDIF                                                  LF + LD
      *
      *  DARTS code must be valid, if entered
      *
     C  N90SC#4DR        IFNE      *BLANKS
     C     SC#4DR        CHAIN     PCT01                              60
      *
     C     *IN60         IFEQ      *ON
     C     CTSTAT        OREQ      'I'
     C                   SETON                                        90  35
     C                   ENDIF                                                  DART Code
      *
     C                   ENDIF                                                  SC#4DR
      *
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
      *
      *  CX Cover Letter
      *
      *    If Folder or Document entered
      *
     C  N90SCCCLF        IFNE      *BLANKS
     C     SCCCLD        ORNE      *BLANKS
      ***
      *
      *    Folder and Document mutually inclusive
      *
     C  N90SCCCLF        IFNE      *BLANKS
     C     SCCCLD        ANDEQ     *BLANKS
     C     SCCCLF        OREQ      *BLANKS
     C     SCCCLD        ANDNE     *BLANKS
     C                   SETON                                        90  71
     C                   ENDIF
      ***
      *
      *    Folder must be valid
      *
     C  N90SCCCLF        IFNE      *BLANKS
     C                   MOVEL(P)  SCCCLF        ##FLR
      *
     C                   CALL      'SVALFLR'
     C                   PARM                    ##FLR            80
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RTCD        IFNE      *BLANK
     C                   SETON                                        90  72
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  LF valid
      ***
      *
      *    Document must be valid
      *
     C  N90SCCCLD        IFNE      *BLANKS
     C                   MOVEL(P)  SCCCLD        ##DCID
     C                   MOVEL(P)  SCCCLF        ##FLR
      *
     C                   CALL      'SVALDOC'
     C                   PARM                    ##DCID           12
     C                   PARM                    ##FLR            80
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RTCD        IFEQ      *ON
     C                   SETON                                        90  73
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  LD
      ***
     C                   ENDIF                                                  LF + LD
      *
      *  CX Reason code must be valid if entered
      *
     C  N90SCCCRS        IFNE      *BLANKS
     C                   MOVEL     'CXCD'        KKTBID
     C                   MOVEL(P)  SCCCRS        KKKEY
     C     GTKY          CHAIN     PGT01                              74
     C   74              SETON                                        90
     C                   ENDIF                                                  SCCCRS
      ********
      ********
      *
      *  Client Specific CX Confirmation Cover Letter
      *
      *    If Folder or Document entered
      *
     C  N90SCCFLF        IFNE      *BLANKS
     C     SCCFLD        ORNE      *BLANKS
      ***
      *
      *    Folder and Document mutually inclusive
      *
     C  N90SCCFLF        IFNE      *BLANKS
     C     SCCFLD        ANDEQ     *BLANKS
     C     SCCFLF        OREQ      *BLANKS
     C     SCCFLD        ANDNE     *BLANKS
     C                   SETON                                        90  75
     C                   ENDIF
      ***
      *
      *    Folder must be valid
      *
     C  N90SCCFLF        IFNE      *BLANKS
     C                   MOVEL(P)  SCCFLF        ##FLR
      *
     C                   CALL      'SVALFLR'
     C                   PARM                    ##FLR            80
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RTCD        IFNE      *BLANK
     C                   SETON                                        90  76
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  LF valid
      ***
      *
      *    Document must be valid
      *
     C  N90SCCFLD        IFNE      *BLANKS
     C                   MOVEL(P)  SCCFLD        ##DCID
     C                   MOVEL(P)  SCCFLF        ##FLR
      *
     C                   CALL      'SVALDOC'
     C                   PARM                    ##DCID           12
     C                   PARM                    ##FLR            80
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RTCD        IFEQ      *ON
     C                   SETON                                        90  77
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  LD
      ***
     C                   ENDIF                                                  LF + LD
      *
      ********
      ********
      *
      *  Client Specific CX Certificate Letter
      *
      *    If Folder or Document entered
      *
     C  N90SCCSCF        IFNE      *BLANKS
     C     SCCSCD        ORNE      *BLANKS
      ***
      *
      *    Folder and Document mutually inclusive
      *
     C  N90SCCSCF        IFNE      *BLANKS
     C     SCCSCD        ANDEQ     *BLANKS
     C     SCCSCF        OREQ      *BLANKS
     C     SCCSCD        ANDNE     *BLANKS
     C                   SETON                                        90  79
     C                   ENDIF
      ***
      *
      *    Folder must be valid
      *
     C  N90SCCSCF        IFNE      *BLANKS
     C                   MOVEL(P)  SCCSCF        ##FLR
      *
     C                   CALL      'SVALFLR'
     C                   PARM                    ##FLR            80
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RTCD        IFNE      *BLANK
     C                   SETON                                        90  80
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  LF valid
      ***
      *
      *    Document must be valid
      *
     C  N90SCCSCD        IFNE      *BLANKS
     C                   MOVEL(P)  SCCSCD        ##DCID
     C                   MOVEL(P)  SCCSCF        ##FLR
      *
     C                   CALL      'SVALDOC'
     C                   PARM                    ##DCID           12
     C                   PARM                    ##FLR            80
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RTCD        IFEQ      *ON
     C                   SETON                                        90  81
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  LD
      ***
     C                   ENDIF                                                  LF + LD
      ********
      ********
      *
      *  Client Specific Reinstatement Certificate Letter
      *
      *    If Folder or Document entered
      *
     C  N90SCCSRF        IFNE      *BLANKS
     C     SCCSRD        ORNE      *BLANKS
      ***
      *
      *    Folder and Document mutually inclusive
      *
     C  N90SCCSRF        IFNE      *BLANKS
     C     SCCSRD        ANDEQ     *BLANKS
     C     SCCSRF        OREQ      *BLANKS
     C     SCCSRD        ANDNE     *BLANKS
     C                   SETON                                        90  82
     C                   ENDIF
      ***
      *
      *    Folder must be valid
      *
     C  N90SCCSRF        IFNE      *BLANKS
     C                   MOVEL(P)  SCCSRF        ##FLR
      *
     C                   CALL      'SVALFLR'
     C                   PARM                    ##FLR            80
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RTCD        IFNE      *BLANK
     C                   SETON                                        90  83
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  LF valid
      ***
      *
      *    Document must be valid
      *
     C  N90SCCSRD        IFNE      *BLANKS
     C                   MOVEL(P)  SCCSRD        ##DCID
     C                   MOVEL(P)  SCCSRF        ##FLR
      *
     C                   CALL      'SVALDOC'
     C                   PARM                    ##DCID           12
     C                   PARM                    ##FLR            80
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RTCD        IFEQ      *ON
     C                   SETON                                        90  84
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  LD
      ***
     C                   ENDIF                                                  LF + LD
      *
      ********
      ********
      *
      *  Reinstatement Cover Letter
      *
      *    If Folder or Document entered
      *
     C  N90SCRCLF        IFNE      *BLANKS
     C     SCRCLD        ORNE      *BLANKS
      ***
      *
      *    Folder and Document mutually inclusive
      *
     C  N90SCRCLF        IFNE      *BLANKS
     C     SCRCLD        ANDEQ     *BLANKS
     C     SCRCLF        OREQ      *BLANKS
     C     SCRCLD        ANDNE     *BLANKS
     C                   SETON                                        90  78
     C                   ENDIF
      ***
      *
      *    Folder must be valid
      *
     C  N90SCRCLF        IFNE      *BLANKS
     C                   MOVEL(P)  SCRCLF        ##FLR
      *
     C                   CALL      'SVALFLR'
     C                   PARM                    ##FLR            80
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RTCD        IFNE      *BLANK
     C                   SETON                                        90  87
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  LF valid
      ***
      *
      *    Document must be valid
      *
     C  N90SCRCLD        IFNE      *BLANKS
     C                   MOVEL(P)  SCRCLD        ##DCID
     C                   MOVEL(P)  SCRCLF        ##FLR
      *
     C                   CALL      'SVALDOC'
     C                   PARM                    ##DCID           12
     C                   PARM                    ##FLR            80
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RTCD        IFEQ      *ON
     C                   SETON                                        90  89
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  LD
      ***
     C                   ENDIF                                                  LF + LD
      *
      *  WE HAVE PASSED ALL EDITS
      *
     C                   ENDSR                                                  ERRCK3
      *** Beg Add ***** 06/21/12 **************************************
      ****************************************************************
      *  ERRCK4 - SUBROUTINE FOR ERROR CHECKING SCR04                *
      ****************************************************************
      /Free
       BegSr  Errck4;
       Endsr;
      /End-Free
      ****************************************************************
      *  ERRCK5 - SUBROUTINE FOR ERROR CHECKING SCR03                *
      ****************************************************************
      /Free
       BegSr  Errck5;
       Endsr;
      /End-Free
      *** End Add ***** 06/21/12 **************************************
      *
      *
      *===============================================================
      * KJDO - SEARCH
      *===============================================================
      *
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
      *  1st CX Letter DARTS Code
      *
     C     HFLD          WHENEQ    'SC#1DR'
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RMCD        IFEQ      *BLANKS
     C                   SETON                                        76
     C                   ELSE
     C                   MOVEL(P)  ##RMCD        SC#1DR
     C                   SETON                                        77
     C                   ENDIF
      *
      *  2nd CX Letter DARTS Code
      *
     C     HFLD          WHENEQ    'SC#2DR'
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RMCD        IFEQ      *BLANKS
     C                   SETON                                        86
     C                   ELSE
     C                   MOVEL(P)  ##RMCD        SC#2DR
     C                   SETON                                        87
     C                   ENDIF
      *
      *  3rd CX Letter DARTS Code
      *
     C     HFLD          WHENEQ    'SC#3DR'
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RMCD        IFEQ      *BLANKS
     C                   SETON                                        26
     C                   ELSE
     C                   MOVEL(P)  ##RMCD        SC#3DR
     C                   SETON                                        27
     C                   ENDIF
      *
      *  4th CX Letter DARTS Code
      *
     C     HFLD          WHENEQ    'SC#4DR'
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RMCD        IFEQ      *BLANKS
     C                   SETON                                        36
     C                   ELSE
     C                   MOVEL(P)  ##RMCD        SC#4DR
     C                   SETON                                        37
     C                   ENDIF
      *
      *  CX cover letter NCCI CX code
      *
     C     HFLD          WHENEQ    'SCCCRS'
     C                   MOVEL     OPTNM         #1PRDS
     C                   CALL      'W2493R'
     C                   PARM                    #1PRDS           40
     C                   PARM      *BLANK        #1CALL            1
     C                   PARM      'CXCD'        #1ID              4
     C                   PARM      *BLANKS       #1KEY            10
     C                   PARM      *BLANK        #1RTC             1
      *
     C     #1KEY         IFEQ      *BLANKS
     C                   SETON                                        85
     C                   ELSE
     C                   SETON                                        86
     C                   MOVEL(P)  #1KEY         SCCCRS
     C                   ENDIF                                                  #1KEY
      *
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  KJDO
      *
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      XCST          SCST
     C                   Z-ADD     XCNOTC        SCNOTC
     C                   Z-ADD     XCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   MOVE      *BLANKS       SCSTNM
     C                   MOVE      *BLANKS       SCSTN2
     C                   MOVEL     XCSTNM        SCSTNM
     C                   MOVE      XCSTNM        SCSTN2
     C*
     C                   MOVE      *BLANKS       SCDREM
     C                   MOVE      *BLANKS       SCDRM2
     C                   MOVEL     XCDREM        SCDREM
     C                   MOVE      XCDREM        SCDRM2
     C                   MOVEL     XCCXFL        SCCXFL
     C                   MOVE      XCCXDC        SCCXDC
     C                   MOVEL     XCREFL        SCREFL
     C                   MOVE      XCREDC        SCREDC
      *
     C                   Z-ADD     XCCXLM        SCCXLM
     C                   Z-ADD     XCCXMN        SCCXMN
      *
     C                   MOVEL(P)  XC#1LF        SC#1LF
     C                   MOVEL(P)  XC#1LD        SC#1LD
     C                   MOVEL(P)  XC#1DR        SC#1DR
      *
     C                   MOVEL(P)  XC#2LF        SC#2LF
     C                   MOVEL(P)  XC#2LD        SC#2LD
     C                   MOVEL(P)  XC#2DR        SC#2DR
      *
     C                   MOVEL(P)  XC#3LF        SC#3LF
     C                   MOVEL(P)  XC#3LD        SC#3LD
     C                   MOVEL(P)  XC#3DR        SC#3DR
      *
     C                   MOVEL(P)  XC#4LF        SC#4LF
     C                   MOVEL(P)  XC#4LD        SC#4LD
     C                   MOVEL(P)  XC#4DR        SC#4DR
      *
      *
     C                   MOVEL(P)  XCCCLF        SCCCLF
     C                   MOVEL(P)  XCCCLD        SCCCLD
     C                   MOVEL(P)  XCCCRS        SCCCRS
      *
     C                   MOVEL(P)  XCCFLF        SCCFLF
     C                   MOVEL(P)  XCCFLD        SCCFLD
      *
     C                   MOVEL(P)  XCCSCF        SCCSCF
     C                   MOVEL(P)  XCCSCD        SCCSCD
      *
     C                   MOVEL(P)  XCCSRF        SCCSRF
     C                   MOVEL(P)  XCCSRD        SCCSRD
      *
     C                   MOVEL(P)  XCRCLF        SCRCLF
     C                   MOVEL(P)  XCRCLD        SCRCLD
      *
      ***** BEG ADD ******* 06/21/12 **********************************
      /Free
       scRrCd = XCrrCd ;    // Move the Data to Screen Field
       //*** Beg Add ****** 09/05/18 **********************************
       scGenStCx = XCstCx ;    //Generate state CX transaction on state removal
       //*** End Add ****** 09/05/18 **********************************
      /End-Free
      ***** END ADD ******* 06/21/12 **********************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     XXCO#         XCCO#
     C                   Z-ADD     XXFND         XCFND
     C                   MOVE      SCST          XCST
     C                   Z-ADD     SCNOTC        XCNOTC
     C                   MOVE      *BLANKS       XCSTNM
     C                   MOVEL     SCSTNM        XCSTNM
     C                   MOVE      SCSTN2        XCSTNM
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XCEFDT
     C                   MOVE      *BLANKS       XCDREM
     C                   MOVEL     SCDREM        XCDREM
     C                   MOVE      SCDRM2        XCDREM
     C                   MOVEL     SCCXFL        XCCXFL
     C                   MOVE      SCCXDC        XCCXDC
     C                   MOVEL     SCREFL        XCREFL
     C                   MOVE      SCREDC        XCREDC
      *
     C                   Z-ADD     SCCXLM        XCCXLM
     C                   Z-ADD     SCCXMN        XCCXMN
      *
     C                   MOVEL(P)  SC#1LF        XC#1LF
     C                   MOVEL(P)  SC#1LD        XC#1LD
     C                   MOVEL(P)  SC#1DR        XC#1DR
      *
     C                   MOVEL(P)  SC#2LF        XC#2LF
     C                   MOVEL(P)  SC#2LD        XC#2LD
     C                   MOVEL(P)  SC#2DR        XC#2DR
      *
     C                   MOVEL(P)  SC#3LF        XC#3LF
     C                   MOVEL(P)  SC#3LD        XC#3LD
     C                   MOVEL(P)  SC#3DR        XC#3DR
      *
     C                   MOVEL(P)  SC#4LF        XC#4LF
     C                   MOVEL(P)  SC#4LD        XC#4LD
     C                   MOVEL(P)  SC#4DR        XC#4DR
      *
      *
     C                   MOVEL(P)  SCCCLF        XCCCLF
     C                   MOVEL(P)  SCCCLD        XCCCLD
     C                   MOVEL(P)  SCCCRS        XCCCRS
      *
     C                   MOVEL(P)  SCCFLF        XCCFLF
     C                   MOVEL(P)  SCCFLD        XCCFLD
      *
     C                   MOVEL(P)  SCCSCF        XCCSCF
     C                   MOVEL(P)  SCCSCD        XCCSCD
      *
     C                   MOVEL(P)  SCCSRF        XCCSRF
     C                   MOVEL(P)  SCCSRD        XCCSRD
      *
     C                   MOVEL(P)  SCRCLF        XCRCLF
     C                   MOVEL(P)  SCRCLD        XCRCLD
      *
      ***** BEG ADD ******* 06/21/12 **********************************
      /Free
       XCrrCd = scRrCd;    // Move Data from Screen to file.
       //*** Beg Add ****** 09/05/18 **********************************
       XCstCx = scGenStCx;    //Generate state CX transaction on state removal
       //*** End Add ****** 09/05/18 **********************************
      /End-Free
      ***** END ADD ******* 06/21/12 **********************************
     C                   ENDSR                                                  MVSCDB
      *
     C*
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
       //*** begin add ***  10/06/16  ****************************************
      /free

       isScriptCX = UseScriptura(processTypeC);
       isScriptRE = UseScriptura(processTypeR);

       Clear tipScriptC;
       Clear tipScriptR;

       If isScriptCX = *on;
          tipScriptC = 'Scriptura has been enabled for cancellations in ' +
           'Application Configuration.  Templates are specified there.';
       Endif;

       If isScriptRE = *on;
          tipScriptR = 'Scriptura has been enabled for reinstatements in ' +
           'Application Configuration.  Templates are specified there.';
       Endif;

      /end-free
       //*** end add *****  10/06/16  ****************************************
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C*
     C                   MOVE      *OFF          *IN92
     C                   MOVE      *OFF          *IN40
     C*
     C     @@MODE        IFEQ      'A'
     C                   MOVE      *ON           *IN40
     C                   ENDIF                                                  @@MODE EQ A
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C                   Z-ADD     @@CO#         XXCO#
     C                   Z-ADD     @@FND         XXFND
     C                   Z-ADD     @@FND         SCFND
     C                   MOVE      XCST          XXST
     C                   Z-ADD     XCEFDT        XXEFDT
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNNAME        SCFNAM
     C                   ENDIF                                                  *IN60 EQ 0
     C*
     C                   EXSR      MVDBSC
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B02  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        XCUSER
     C                   MOVEL     Q1PGM         XCPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XCDT
     C                   MOVEL     DSTIME        XCTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E02  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1PGM         XCPGM
     C                   MOVE      Q1USER        XCUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XCDT
     C                   MOVEL     DSTIME        XCTIME
     C                   MOVEL     Q1PGM         XCIPGM
     C                   MOVEL     SRDTO         XCIDT
     C                   MOVEL     DSTIME        XCITIM
     C                   MOVE      Q1USER        XCIUSR
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WTXCP
     C*
     C                   MOVEL     'C'           @@MODE
     C*
     C                   ENDSR                                                  WRITER
