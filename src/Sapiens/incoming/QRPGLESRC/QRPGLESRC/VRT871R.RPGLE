     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* *****************************************************************
     H** REVISIONS:
     H*                                                               *
     H* 12/05/15  C:33  Project: SY195                                *
     H*                 Remove QDLS hardcoding                        *
     H*                                                               *
     H* 04/15/14  C:300 Project: SY195                                *
     H*                 Remove SY195 Changes (11/02/10)               *
     H*                                                               *
     H* 09/16/13  C:33  Project: SY195                                *
     H*                 Allow for more than 5 positions for the root  *
     H*                                                               *
     H* 09/05/13 C:062 Project ID132 82619                            *
     H*                CHECK SMOCP IF NOT HIT ON SMNOP                *
    H*                                                                   *
    H* 11/02/10  C:183 Project: SY195                                    *
    H*                 Remove hard coding for QDLS. Call RTVDOCROOT to   *
    H*                 get the root folder name.                         *
     H**
     H****  02 - AOOPTICAL - DATACENTER PROCESSING TO BE ADDED FOR
     F***                    OPTICAL
     H**   10/10/06 -INCLUDE DATACENTER PREFIX PROCESSING
     H**
      *
      *  07/22/05  C:002 SUP#48184
      *
      *                CHK FOR DTM FLAG, IF FOUND DO NOT CONVERT DOC  *
      *
     H* 09/29/04 C:062 Project WC114 ?????                            *
     H*                ADD UNIQUE FOLDER TO VRTCVTD                   *
     H*                                                               *
     H* 07/21/04 C:062 Project WC129 43306                            *
     H*                CREATE FOLDER                                  *
     H*                                                               *
     H* 07/12/04 C:062 Project DR187                                  *
     H*                Don't copy the art file.                       *
     H*                                                               *
     H* 07/09/04 C:062 Project DR190                                  *
     H*                Make name unique, use time in folder           *
     H*                named by job.                                  *
     H*                                                               *
     H* 07/01/04 C:062 Project SY100                                  *
     H*                Add Data Center Processing                     *
     H*
     H* C:02    120IAD   -  MODIFY TO GET THE SMOCP FILE TO RETRIEVE
     H*                   01/26/04    THE OPTICAL LIBRARY NAME
     H*******************************************************************
      ****>>>> RETRIEVE DOCUMENTS FROM OPTICAL <<<<<******
      *
      ** ** ** MAKE SURE DFRWRT(*NO) IS SPECIFIED ON THE 870F DSPF!!
      *
      *
     FSMDLL     IF   E           K DISK
     FSDCMLE    IF   E           K DISK
     F*
     F**** ADD 01/26/04 ***********************************************
     FSMOCP     IF   E             DISK
     F                                     RENAME(POC01:POC02)
     F**** END ADD 01/26/04 *******************************************
     F*** BEGIN ADD *** 09/05/13 **************************************
     FSMNOL     IF   E           K DISK
     F*** END   ADD *** 09/05/13 **************************************
      *

     D*
     D*** CHANGE 1/26/04 *** GET LIBN FROM SMOCP INSTEAD
     D*** DEL       '/OPTMLB01'           C         OPTDIR
     D*** Begin Chg *** 07/09/04 **************************************
     D**********XXDCID      DS
     D**********                              1   1 TPRE
     D**********                              2   50TJOB#
     D**********                              6   80THHM
     D**********                              9   9 TPERD
     D**********                             10  120TMSS
     D XXDCID          DS
     D  QTIM                   1      8
     D  TPERD                  9      9
     D  TSFX                  10     12
     D*** End   Chg *** 07/09/04 **************************************
     D SPATH           DS
     D  SPFSYS                 1      9
     D**************************              1   5 SPFSYS
     D  SVOLPT                10     80
     D**************************              6  80 SVOLPT
     D SPTHLN          DS
     D  SPTHL                  1      4B 0
     D*
     D***  Begin Chg  ***  09/16/13  ***********************************
     D*TPATH****       DS
     D**TPFSYS**               1      5
     D**TVOLPT**               6     80
     DtPath            s             80
     DtVolPt           s             80
     D***  End   Chg  ***  09/16/13  ***********************************
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
     D****************************        B   1   40RCLEN
     D***************************         B   5   80RTCD
     D  RTCD                   1      8
     D  EXID                   9     15
     D  CONDTN                 9     15
     D  RSV                   16     16
     D  EXDTA                 17    272
     D  MSG                   17    272
      *** Beg add ***** 11/02/10 ***********************************************
     C***** beg del ****** 04/15/14 **********************************
     D***** RPGM            C                   CONST('RTVDOCROOT')
     C***** end del ****** 04/15/14 **********************************
      *** End add ***** 11/02/10 ***********************************************
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
     D***  Begin Add  ***  12/05/15  ****
     DRtvDocRoot       pr                  extPgm('RTVDOCROOT')
     D inRoot                        10

     DstrRoot          s             10
     D***  End   Add  ***  12/05/15  ****

     C*
      ** ** PARM LISTS  ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **
      *
      * * * THE PARM LIST FOR CALLING THE COPY STREAM FILE API
      *
     C     PCPYSF        PLIST
     C                   PARM                    SPATH            80
     C                   PARM                    SPTHL
     C                   PARM                    CPYINF
     C                   PARM                    TPATH            80
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
      ** ** KEY LIST ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **
     C*** BEGIN ADD *** 09/05/13 **************************************
     C     NOKY          KLIST
     C                   KFLD                    DLOSYS
     C*** END   ADD *** 09/05/13 **************************************
      *
     C     DLKY          KLIST
     C                   KFLD                    QQKY
     C                   KFLD                    XXSEQ#
     C*
      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **
     C*
      * * * VERIFY THAT THE REQUESTED DOC IS ON OPTICAL.
      * * * IF SO, GET THE NEEDED INFO.  DISPLAY THE WINDOW FOR "WAIT", AND
      * * * PROCESS THE OPTICAL RETRIEVE. (WE WILL GET THE NAME OF THE DOC
      * * * TO BUILD, AND FOLDER FROM A CONTROL FILE.  IF AN ERROR OCCURS,
      * * * WE MUST RETURN AN NON-BLANK VALUE IN RTCD!!!
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKY             40
     C                   PARM                    QQSQ#             5
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM                    QQRTCD            1
     C*** BEGIN ADD *** 07/12/04 **************************************
     C                   PARM                    QQADOC           12
     C                   PARM                    QQAFLD           80
     C*** END   ADD *** 07/12/04 **************************************
     C*** BEGIN ADD *** 07/01/04 **************************************
     C                   CALL      'SA030R'
     C                   PARM      *BLANKS       ##PRFX            3
     C                   PARM      *BLANKS       ##PFLR           80
     C*** END   ADD *** 07/01/04 **************************************
      *** Beg add ***** 11/02/10 ***********************************************
     C***** beg del ****** 04/15/14 **********************************
     C*****              CALL      RPGM
     C*****              PARM                    ROOT             10
     C***** end del ****** 04/15/14 **********************************
      *** End add ***** 11/02/10 ***********************************************
     C***  Begin Add  ***  12/05/15  ****
      /Free
       RtvDocRoot(strRoot);
      /End-free
     C***  Begin Add  ***  12/05/15  ****
     C*
     C*** Begin Add *** 07/09/04 **************************************
     C                   MOVE      QQRTCD        XXOTYP            1            ** OBJ TYPE, IF O ?
     C                   MOVE      *BLANKS       QQRTCD
     C*** End   Add *** 07/09/04 **************************************
     C                   MOVE      QQSQ#         XXSEQ#            5 0
     C                   MOVE      *BLANKS       MSGDSC          256
     C*** BEGIN DEL *** 09/05/13 **************************************
     C*** 01/26/04 ** ADD *****************************************8
     C*****1****         CHAIN     SMOCP                              64
     C******IN64         IFEQ      *OFF
     C**********         MOVEL     OCOSYS        QQOSYS            1
     C**********         MOVEL     OCLIBN        QQLIBN           10
     C**********         ENDIF
     C*
     C*****'/'**         CAT(P)    QQLIBN:0      OPTDIR           11
     C*** 01/26/04 ** END ADD *************************************8
     C*** END   DEL *** 09/05/13 **************************************
     C*
      *************************************************************
     C*
      *** GET DOC LOCATION RECORD
      *
     C     DLKY          CHAIN     PDL01                              60
      *
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN10
     C                   MOVE      '1'           QQRTCD                         NO DL RCD FOUND
     C                   GOTO      TERM
     C                   END
     C*** BEGIN ADD *** 09/05/13 **************************************
     C     NOKY          CHAIN     PNO01                              64
     C     *IN64         IFEQ      *OFF
     C                   MOVEL     NOOSYS        QQOSYS            1
     C                   MOVEL     NOLIBN        QQLIBN           10
     C                   ELSE
     C     1             CHAIN     SMOCP                              64
     C     *IN64         IFEQ      *OFF
     C                   MOVEL     OCOSYS        QQOSYS
     C                   MOVEL     OCLIBN        QQLIBN
     C                   ENDIF
     C                   ENDIF
     C*
     C     '/'           CAT(P)    QQLIBN:0      OPTDIR           11
     C*** END   ADD *** 09/05/13 **************************************
      *
      *
      *** END IN ERROR IF NOT A VALID OPTICAL RECORD.
      *
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
     C*********            MOVEL'/QDLS'   SPFSYS           FROM OPTICAL
     C                   MOVEL     OPTDIR        SPFSYS                         FROM VRTOPT
      *** Beg chg ***** 11/02/10 ***********************************************
     C*******              MOVEL'/QDLS'   TPFSYS           TO   DASD
     C***  Begin Chg  ***  09/16/13  *******************************************
     C*****'/'**         CAT       ROOT          TPFSYS                         TO   DASD
      /Free
       //** beg chg ****** 04/15/14 **********************************
       //*****   tPath = '/' + %trim(root) + %trim(tVolPt);
       //tPath = '/QDLS' + %trim(tVolPt);
       tPath = '/' +%trim(strRoot) + %trim(tVolPt);
       //** end chg ****** 04/15/14 **********************************
      /End-free
     C***  End   Chg  ***  09/16/13  *******************************************
      *** End chg ***** 11/02/10 ***********************************************
      *
      * * * BUILD DOCUMENT NAME (UNIQUE) "T" + JOB#(LAST 4 #'S) +
      * * *  TIME(HHM.MSS)  (TO)
     C                   TIME                    DSDTTM
     C*** Begin Add *** 07/09/04 **************************************
     C                   CALL      'SRGETTIM'
     C                   PARM      *BLANKS       QTIM
     C*
     C     QTIM          IFEQ      *BLANKS
     C                   MOVEL     '00000000'    QTIM
     C                   ENDIF
     C*** End   Add *** 07/09/04 **************************************
     C*** Begin Del *** 07/09/04 **************************************
     C**********           MOVEL'T'       TPRE
     C**********           MOVE Q1JOBN    TJOB#
     C**********           MOVELDSTIME    THHM
     C*** End   Del *** 07/09/04 **************************************
     C                   MOVEL     '.'           TPERD
     C*** Begin Del *** 07/09/04 **************************************
     C**********           MOVE DSTIME    TMSS
     C*** End   Del *** 07/09/04 **************************************
     C*** Begin Add *** 07/09/04 **************************************
     C*
     C*  IF PROCESSING AN "O" TYPE (OBJECT) THEN DO THIS...
     C*  CHANGE PROGRAM TO USE ORIGINAL SUFFIX FROM DOC (TSFX)
     C*
     C                   Z-ADD     *ZERO         G
     C     '.'           SCAN      QQDCID:1      G                 2 0    43
     C     *IN43         IFEQ      *ON                                          FOUND
     C                   ADD       1             G
     C     3             SUBST     QQDCID:G      TSFX              3    44
     C     *IN44         IFEQ      *ON                                          ERROR
     C                   MOVEL     'TXT'         TSFX
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     'TXT'         TSFX
     C                   ENDIF
     C*** End   Add *** 07/09/04 **************************************
     C***  CREATE THE TO  PATH
     C*** BEGIN ADD *** 07/01/04 **************************************
     C     ##PFLR        IFNE      *BLANKS
     C     '/'           CAT       'OPTTEMP'     OPTDR             8
     C     '/'           CAT(P)    ##PFLR:0      TVOLPT
     C                   CAT       OPTDR:0       TVOLPT
     C                   ELSE
     C*** END   ADD *** 07/01/04 **************************************
     C     '/'           CAT(P)    'OPTTEMP'     TVOLPT
     C*** BEGIN ADD *** 07/01/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 07/01/04 **************************************
     C*** Begin Add *** 07/09/04 **************************************
     C                   MOVE      Q1JOBN        JOBNBR            6
     C                   CAT(P)    '/':0         TVOLPT
     C                   CAT(P)    JOBNBR:0      TVOLPT
     C*** End   Add *** 07/09/04 **************************************
     C*** BEGIN ADD *** 07/12/04 **************************************
     C     Q1PRMS        IFGT      5
     C                   MOVEL     QQDCID        QQADOC
     C                   MOVE      '.ART'        QQADOC
     C                   MOVEL(P)  QQFOLD        QQAFLD
     C                   ENDIF
     C*** END   ADD *** 07/12/04 **************************************
     C***  Begin Add  ***  09/16/13  *******************************************
      /Free
       //** beg chg ****** 04/15/14 **********************************
       //*****   tPath = '/' + %trim(root) + %trim(tVolPt);
       //tPath = '/QDLS' + %trim(tVolPt);
       tPath = '/' +%trim(strRoot) + %trim(tVolPt);
       //** end chg ****** 04/15/14 **********************************
      /End-free
     C***  End   Add  ***  09/16/13  *******************************************
     C*** BEGIN ADD *** 07/21/04 **************************************
     C                   CALL      'SC871C'
     C                   PARM                    TPATH
     C*** END   ADD *** 07/21/04 **************************************
     C                   CAT(P)    '/':0         TVOLPT
     C                   CAT(P)    XXDCID:0      TVOLPT
     C***  Begin Add  ***  09/16/13  *******************************************
      /Free
       //** beg chg ****** 04/15/14 **********************************
       //*****   tPath = '/' + %trim(root) + %trim(tVolPt);
       //tPath = '/QDLS' + %trim(tVolPt);
       tPath = '/' +%trim(strRoot) + %trim(tVolPt);
       //** end chg ****** 04/15/14 **********************************
      /End-free
     C***  End   Add  ***  09/16/13  *******************************************
      *
      *   BUILD VOLUME AND PATH STRINGS BY CONCATENATING VOLUME WITH PATH
      *   ETC.
      *
      *** MOVE DL VALUES TO QQ(PARM LIST) VALUES FOR CALL
      *
      *
     C***  CREATE THE SOURCE PATH
     C     '/'           CAT(P)    DLVOL1:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    DLOFLD:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    DLOSBD:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    DLDCID:0      SVOLPT
      *
     ***  ADD 10/10/06 ****************************************************
     C     ##PFLR        IFNE      *BLANKS
     C     '/'           CAT(P)    ##PFLR:0      @@WK3            80
     C                   CAT(P)    OPTDIR:0      @@WK3
     C                   CAT(P)    SVOLPT:0      @@WK3
     C                   MOVEL     @@WK3         SPATH
     C                   ENDIF
     ***  END ADD 10/10/06 ****************************************************
     C                   Z-ADD     80            SPTHL                          PTH LEN
     C                   Z-ADD     80            TPTHL                          PTH LEN
      *
     C                   MOVEL     '1'           TYPOFC                         TYPE OF COPY
     C                   Z-ADD     272           RCLEN            15 5          LEN OF ERROR CD
      *
     C******************   CALL 'QHFCPYSF'PCPYSF           CPY DOC TO DASD
     C                   CALL      'VRTCPYC'
     C                   PARM                    SPATH
     C                   PARM                    TPATH
     C                   PARM      *BLANKS       RETCD
      *
      *
      *
      *
      *
     C*** CHECK ERROR STUFF ***
     C*
     C     RTCD          IFNE      *BLANKS
     C     CONDTN        ANDNE     'CPF8A82'
     C*
     C                   MOVE      '1'           ERR               1
     C                   MOVEL     *BLANKS       QQDCID
     C                   MOVEL     *BLANKS       QQFOLD
     C*** PROCESS ERRORS...RETURN THEM TO THE JOB LOG. ETC.
     C*
      * * * DEFAULT ERROR TO RETURN
     C                   MOVE      '3'           QQRTCD
     C*
      * * * VOLUME FULL(THRESHOLD MET)
     C     CONDTN        IFEQ      'CPF1F61'
     C                   MOVE      '1'           QQRTCD
     C                   END
     C*
      * * * PATH NAME INVALID
     C     CONDTN        IFEQ      'CPF1F48'
     C                   MOVE      '2'           QQRTCD
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
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   END
     C*
     C                   ELSE
     C                   MOVEL     XXDCID        QQDCID
     C                   MOVEL(P)  'OPTTEMP'     QQFOLD
     C*** Begin Add *** 07/09/04 **************************************
     C                   CAT(P)    '/':0         QQFOLD
     C                   CAT(P)    JOBNBR:0      QQFOLD
     C*** End   Add *** 07/09/04 **************************************
     C                   MOVEL     *BLANKS       QQRTCD
     C                   END
      *
      *
      *
     C**** CONVERT TO FFTDCA???
     C*                   COULD GET CM RCD TO DETERMINE WHAT TO CVT TO
     C*** ONLY CONVERT IF OV DOC.. ALL OTHERS ARE AS IS
      *
     C     DLKY          CHAIN     PCM01                              67
     C     *IN67         IFEQ      *OFF
     C                   MOVE      CMIMGF        XXIMGF            1
     C                   ELSE
     C                   MOVE      *BLANKS       XXIMGF                                   DFT
     C                   ENDIF
      *
     C     XXIMGF        IFEQ      *BLANK
     C*** BEGIN ADD *** 07/01/04 **************************************
     C     ##PFLR        IFNE      *BLANKS
     C                   MOVEL(P)  'OPTTEMP'     OPTTMP            8
     C     ##PFLR        CAT(P)    '/':0         XXFLD
     C                   CAT       OPTTMP:0      XXFLD
     C*** Begin Add *** 09/29/04 **************************************
     C                   CAT       '/':0         XXFLD
     C                   CAT       JOBNBR:0      XXFLD
     C*** End   Add *** 09/29/04 **************************************
     C                   ELSE
     C*** END   ADD *** 07/01/04 **************************************
     C                   MOVEL(P)  'OPTTEMP'     XXFLD            80
     C*** Begin Add *** 09/29/04 **************************************
     C                   CAT       '/':0         XXFLD
     C                   CAT       JOBNBR:0      XXFLD
     C*** End   Add *** 09/29/04 **************************************
     C*** BEGIN ADD *** 07/01/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 07/01/04 **************************************
     C**** CHANGE 07/22/05 ************************************************
     C*** REMOVE CALL TO VRTCVTD IF NATIVE DTM IS BEING USED ****
     C*
     C                   CALL      'DTMCHK'
     C                   PARM      *BLANK        XDTM              1
     C     XDTM          IFNE      'Y'
     C                   CALL      'VRTCVTD'
     C                   PARM                    XXDCID
     C                   PARM                    XXFLD
     C                   PARM      'FFTDCA  '    XXTYP            10
     C                   PARM      *BLANK        XXRCD             1
     C                   ENDIF                                                          END XDTM
     C*** DEL 7/22/05 **   CALL 'VRTCVTD'
     C*** DEL 7/22/05 **   PARM           XXDCID
     C*** DEL 7/22/05 ***  PARM           XXFLD
     C*** DEL 7/22/05 ***  PARM 'FFTDCA  'XXTYP  10
     C*** DEL 7/22/05 ***  PARM *BLANK    XXRCD   1
     C*
     C*** END CHANGE 07/22/05 ********************************************
     C                   ENDIF
     C*
      *
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
