     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H*    C:02
      ****>>>> RETRIEVE DOCUMENTS FROM OPTICAL <<<<<******
      *
      *

     D XXDCID          DS
     D  TPRE                   1      1
     D  TJOB#                  2      5  0
     D  THHM                   6      8  0
     D  TPERD                  9      9
     D  TMSS                  10     12  0
     D SPATH           DS
     D  SPFSYS                 1      5
     D  SVOLPT                 6     80
     D SPTHLN          DS
     D  SPTHL                  1      4B 0
     D*
     D TPATH           DS
     D  TPFSYS                 1      5
     D  TVOLPT                 6     80
     D TPTHLN          DS
     D  TPTHL                  1      4B 0
     D*
     D CPYINF          DS
     D  TYPOFC                 1      1
     D  CPYFIL                 2      6
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
      *
     D RETCD           DS
     D  RCLEN                  1      4B 0
     D  RTCD                   5      8B 0
     D  EXID                   9     15
     D  CONDTN                 9     15
     D  RSV                   16     16
     D  EXDTA                 17    272
     D  MSG                   17    272
      *
      *--------------------------------------------------------------------*
      *    RETCD HAS THE FOLLOWING INFO                                    *
      *--------------------------------------------------------------------*
      *** INPUT    BINARY(4)     BYTES PROVIDED   -   0 =EXCEPTION HANDLING
      *                                            else =Error Info rtnd
      *
      *** OUTPUT   BINARY(4)     BYTES AVALIBLE   -   0 =No error was rtrnd
      *                                            else =Length of data rtnd
      *
      *** OUTPUT   CHAR(7)       EXCEPTION ID           =Message ID
      *
      *** OUTPUT   CHAR(1)       RESERVED               =nothing
      *
      *** OUTPUT   CHAR(*)       EXCEPTION DATA         =error data rtnd
      *
      *--------------------------------------------------------------------*
      *  THE PROGRAM MUST HANDLE ALL EXCEPTIONS IF WE PASS A VALUE <> 0
      * IN THE RCLEN FIELD.  RETURNED TO THE PGM ARE THE MESSAGE ID(EXID)
      * AND THE MESSAGE DATA (EXDTA).
     D*
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C*
      ** ** PARM LISTS  ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **
      *
      * * * THE PARM LIST FOR CALLING THE COPY STREAM FILE API
      *
     C     PCPYSF        PLIST
     C                   PARM                    FRMOBJ
     C                   PARM                    SPTHL
     C                   PARM                    CPYINF
     C                   PARM                    TOOBJ
     C                   PARM                    TPTHL
     C                   PARM                    RETCD
     C*
     C     PRMSG         PLIST
     C                   PARM                    QMID              7
     C                   PARM                    QMDTA           256
     C                   PARM                    QMDESC          256
     C                   PARM                    QMRTN             1
     C*
     C*
     C     PLBRK         PLIST
     C                   PARM                    QMID              7
     C                   PARM                    QMDTA           256
     C                   PARM                    QMRTN             1
     C*
      *
     C*
      * * * VERIFY THAT THE REQUESTED DOC IS ON OPTICAL.
      * * * IF SO, GET THE NEEDED INFO.  DISPLAY THE WINDOW FOR "WAIT", AND
      * * * PROCESS THE OPTICAL RETRIEVE. (WE WILL GET THE NAME OF THE DOC
      * * * TO BUILD, AND FOLDER FROM A CONTROL FILE.  IF AN ERROR OCCURS,
      * * * WE MUST RETURN AN NON-BLANK VALUE IN RTCD!!!
     C*
     C     *ENTRY        PLIST
     C                   PARM                    FRMOBJ           80
     C                   PARM                    TOOBJ            80
     C                   PARM                    QQRTCD            1
     C*** ADD 08/15/02 *****
     C                   EXSR      DBGMOD
     C*** END ADD 08/15/02 ****
     C*
     C                   MOVE      *BLANKS       MSGDSC          256
     C*
      *************************************************************
     C*
     C*
      *** CONTINUE PROCESSING
      ***
      *** GET OPTICAL FILE, CPY TO DLS, THEN RETURN WITH RTCD = " "
      *** USE THE NEXT AVAIL DCID/FOLD INFO FROM THE OPTICAL CTL FILE??
      ***
      *** WE MUST WRITE A PR RCD(DELETE REQUEST) FOR THE DOCID/FOLD OF THIS
      *** TEMP DOCUMENT.  WE MUST UPDAT THE DARTS(SDCMP) RCD WITH THE DCID/FOLD
      *** WHERE WE COPIED THE OPTICAL FILE.
      *
     C                   EXSR      OPTRTV
      ***
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
      *==================================================================*
      *  OPTICAL RETRIEVE SUBROUTINE *** CPY STREAM FILE API ***
      *==================================================================*
      *
     C     OPTRTV        BEGSR
      *
      *
     C************ BUILD THE COPY PARAMETERS
      *
      *   SPECIFY THE FROM AND TO FILE SYSTEMS
      *
     C****                 MOVEL'/QOPT'   SPFSYS           FROM OPTICAL
     C****                 MOVEL'/QDLS'   TPFSYS           TO   DASD
      *
      * * * BUILD DOCUMENT NAME (UNIQUE) "T" + JOB#(LAST 4 #'S) +
      * * *  TIME(HHM.MSS)  (TO)
     C*******              TIME           DSDTTM
     C*****                MOVEL'T'       TPRE
     C*****                MOVE Q1JOBN    TJOB#
     C*****                MOVELDSTIME    THHM
     C*****                MOVEL'.'       TPERD
     C*****                MOVE DSTIME    TMSS
     C*
     C*
     C***  CREATE THE TO  PATH
     C*****      '/'       CAT  'OPTTEMP' TVOLPT    P
     C*******              CAT  '/':0     TVOLPT    P
     C*******              CAT  XXDCID:0  TVOLPT    P
      *
      *   BUILD VOLUME AND PATH STRINGS BY CONCATENATING VOLUME WITH PATH
      *   ETC.
      *
      *** MOVE DL VALUES TO QQ(PARM LIST) VALUES FOR CALL
      *
      *
     C***  CREATE THE SOURCE PATH
     C********** '/'       CAT  DOVOL1:0  SVOLPT    P
     C**********           CAT  '/':0     SVOLPT    P
     C************         CAT  DOOFLD:0  SVOLPT    P
     C***********          CAT  '/':0     SVOLPT    P
     C********             CAT  DOOSFL:0  SVOLPT    P
     C***********          CAT  '/':0     SVOLPT    P
     C**************       CAT  DOODCI:0  SVOLPT    P
      *
     C********NEW****************************************************
     C*******              MOVELFRMOBJ    SVOLPT
     C*********            MOVELTOOBJ     TVOLPT
     C****** END NEW ************************************************
     C                   Z-ADD     80            SPTHL                          PTH LEN
     C                   Z-ADD     80            TPTHL                          PTH LEN
      *
     C                   MOVEL     '1'           TYPOFC                         TYPE OF COPY
     C                   Z-ADD     272           RCLEN                          LEN OF ERROR CD
      *
     C                   CALL      'QHFCPYSF'    PCPYSF                         CPY DOC TO DASD
      *
      *
     C*** CHECK ERROR STUFF ***
     C*
     C     RTCD          IFNE      0
     C     CONDTN        ANDNE     'CPF8A82'
     C*
     C                   MOVE      '1'           ERR               1
     C*** PROCESS ERRORS...RETURN THEM TO THE JOB LOG. ETC.
     C*
     C*
      * * * VOLUME FULL(THRESHOLD MET)
     C     CONDTN        IFEQ      'CPF1F61'
     C                   END
     C*
      * * * PATH NAME INVALID
     C     CONDTN        IFEQ      'CPF1F48'
     C                   END
     C*
     C*** CALL RTV MSG PGM TO SND PGM MSG OF ERROR AND RETRIEVE THE
     C*** DESCRIPTION.
     C                   MOVEL     CONDTN        QMID
     C                   MOVEL     MSG           QMDTA
     C                   MOVEL     *BLANKS       QMDESC
     C                   MOVEL     *BLANKS       QMRTN
     C*
     C                   CALL      'RTVMSGC'     PRMSG
     C     QMRTN         IFEQ      *BLANKS
     C                   MOVE      QMDESC        MSGDSC
     C                   ELSE
     C                   MOVE      *BLANKS       MSGDSC
     C                   END
     C*
     C     CONDTN        IFEQ      'CPF1F71'                                          F SYS
     C                   CALL      'BRKMSGC'     PLBRK
     C*
     C     QMRTN         IFEQ      '*'
     C                   GOTO      TERM
     C                   END
     C*
     C                   END
     C*
     C                   ELSE
      *
      *
     C                   END
      *
     C*
     C************ UPDATE APPROPRIATE RECORDS ETC. FOR DOC LOCATION...ETC.
      *
      *** IF NEEDED COULD UPDATE DOC LOCATION RECORD ETC.
      *** MAY LATER WANT TO KEEP TRACK OF THE TEMP DOC IN OPTTEMP.
      ***
      *** WILL NIGHTLY DELETE THE OPTTEMP FOLDER AND RE-CREATE IT.
      *
      *
      *
      *
     C*
      *
     C     ENDOR         ENDSR
     C*** ADD 08/15/02 ******
     C*====================================================================*
      **** DEBUG MODE (OR VIRTUAL OPTICAL PROCESS)
     C     DBGMOD        BEGSR
     C*
     C     ENDDBG        ENDSR
     C*
     C*** END ADD 08/15/02 ******
***********
********Error - No Document Location can be established
