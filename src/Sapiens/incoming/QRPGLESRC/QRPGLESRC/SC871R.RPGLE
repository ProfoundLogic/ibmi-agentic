     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H*===============================================================*
     H** REVISIONS:                                                   *
     H*                                                               *
     H* 12/05/15  C:33  Project: SY195                                *
     H*                 Remove QDLS hardcoding                        *
    H*                                                                *
    H* 04/15/14  C:300 Project: SY195                                 *
    H*                 Remove SY195 Changes (11/02/10)                *
    H*                                                                *
    H* 11/02/10  C:183 Project: SY195                                 *
    H*                 Remove hard coding for QDLS. Call RTVDOCROOT to*
    H*                 get the root folder name.                      *
    H*                                                                *
     H*                                                               *
     H* 07/21/04 C:062 Project WC129 43306                            *
     H*                CREATE FOLDER                                  *
     H*                                                               *
     H* 07/12/04 C:062 Project DR187                                  *
     H*                Don't copy the art file.                       *
     H*                                                               *
     H* 07/01/04 C:062 Project SY100                                  *
     H*                Add Data Center Processing                     *
     H*                                                               *
     H* 05/23/04 - 02 - MAKE NAEM UNIQUE, USE TIME IN FOLDER NAMED BY JOB
     H*                                                               *
     H* 05/24/04 - 02 - MAKE NAEM UNIQUE, USE TIME IN FOLDER NAMED BY JOB
     H*                                                               *
     H* 05/14/04 - 02                                                 *
     H*       ADDDED CODE TO ALLOW OBJ TYPE (IMGF) TO BE PASSED IN.  USE SUFFIX
     H*       OF ORIGINAL DOCUMENT IF TYPE IS O.                      *
     H*       ADDED CODE FOR VIRTUAL OPTICAL PROCESSING               *
     H*                                                               *
     H* 08/15/02 - 02 - RETRIEVE DOCUMENTS FROM OPTICAL               *
     H*                                                               *
     H*===============================================================*
      *
      ** ** ** MAKE SURE DFRWRT(*NO) IS SPECIFIED ON THE 870F DSPF!!
      *
      *
     FSMDLL     IF   E           K DISK
     F*
      *

     D XXDCID          DS
     D*** ADD 5/24/04 *** NEXT LINE *****
     D  QTIM                   1      8
     D******DEL 05/24/04  *****************   1   1 TPRE
     D****                                    2   50TJOB#
     D****                                    6   80THHM
     D  TPERD                  9      9
     D****** END DEL ********************    10  120TMSS
     D**** ADD 5/14/04 ****
     D  TSFX                  10     12
     D*** END ADD 5/14/04 ***
     D SPATH           DS
     D  SPFSYS                 1      5
     D  SVOLPT                 6     80
     D SPTHLN          DS
     D  SPTHL                  1      4B 0
     D*
     D***  Begin Chg  ***  12/05/15  ******
     D*TPATH****       DS
     D**TPFSYS**               1      5
     D**TVOLPT**               6     80
     DtPath            s             80
     DtVolPt           s             80
     D***  End   Chg  ***  12/05/15  ******
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
      * Call the Root determination program
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
     C*** ADD 05/14/04 ****
     C                   MOVE      QQRTCD        XXOTYP            1            ** OBJ TYPE, IF O ?
     C                   MOVE      *BLANKS       QQRTCD
     C**** END ADD 05/14/03 *****
     C                   MOVE      QQSQ#         XXSEQ#            5 0
     C                   MOVE      *BLANKS       MSGDSC          256
     C*** ADD 08/15/02 *****
     C                   EXSR      DBGMOD
     C*** END ADD 08/15/02 ****
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
     C                   MOVEL     '/QOPT'       SPFSYS                         FROM OPTICAL
      *** Beg chg ***** 11/02/10 ***********************************************
     C********             MOVEL'/QDLS'   TPFSYS           TO   DASD
     C***** beg chg ****** 04/15/14 **********************************
     C*****     '/'           CAT       ROOT          TPFSYS                    TO   DASD
     C* * * *            MOVEL     '/QDLS'       TPFSYS                         TO   DASD
     C***** end chg ****** 04/15/14 **********************************
      *** End chg ***** 11/02/10 ***********************************************
      *
      * * * BUILD DOCUMENT NAME (UNIQUE) "T" + JOB#(LAST 4 #'S) +
      * * *  TIME(HHM.MSS)  (TO)
     C                   TIME                    DSDTTM
     C** ADD 5/24/04 *********************8
     C                   CALL      'SRGETTIM'
     C                   PARM      *BLANKS       QTIM
     C     QTIM          IFEQ      *BLANKS
     C                   MOVEL     '00000000'    QTIM
     C*
     C                   ENDIF
     C*** END ADD 5/24/04 ********************************8
     C*
     C*** DEL NXT LINES 5/24/04 *************************
     C*******              MOVEL'T'       TPRE
     C*******              MOVE Q1JOBN    TJOB#
     C*********            MOVELDSTIME    THHM
     C                   MOVEL     '.'           TPERD
     C*****                MOVE DSTIME    TMSS
     C*** END DEL 5/24/04 **********************************
     C*
     **** C:02 05/14/04 ************** ADD ************ ***********
     C*** IF PROCESSING AN "O" TYPE (OBJECT) THEN DO THIS...
     **  CHANGE PROGRAM TO USE ORIGINAL SUFFIX FROM DOC (TSFX)
     **
     C*** BEGIN DEL *** 07/29/04 **************************************
     C********** XXOTYP    IFEQ 'O'
     C*** END   DEL *** 07/29/04 **************************************
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
     C***
     C*** ADD 5/24/04 ********************
     C*** BEGIN DEL *** 07/29/04 **************************************
     C***********          ELSE
     C***********          MOVEL'000'     TSFX
     C***********          ENDIF
     C*** END   DEL *** 07/29/04 **************************************
     **** END ADD * 5/24/04 ***********************************
     **
     **** END ADD *************************************************
     C*
     C*
     C***  CREATE THE TO  PATH
     C*** DEL NEXT LINE 05/23/04 *************************************
     C***DEL     '/'       CAT  'OPTTEMP' TVOLPT    P
      *** Begin ADD    05/23/04 ***************************************
     C                   MOVE      Q1JOBN        JOBNBR            6
     C*
     C*** BEGIN ADD *** 07/01/04 **************************************
     C     ##PFLR        IFNE      *BLANKS
     C     '/'           CAT       'OPTTEMP'     OPTDIR            8
     C     '/'           CAT(P)    ##PFLR:0      TVOLPT
     C                   CAT       OPTDIR:0      TVOLPT
     C                   ELSE
     C*** END   ADD *** 07/01/04 **************************************
     C     '/'           CAT(P)    'OPTTEMP'     TVOLPT
     C*** BEGIN ADD *** 07/01/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 07/01/04 **************************************
     C                   CAT(P)    '/':0         TVOLPT
     C                   CAT(P)    JOBNBR:0      TVOLPT
      *** End   ADD    05/23/04 ***************************************
     C*** BEGIN ADD *** 07/12/04 **************************************
     C     Q1PRMS        IFGT      5
     C                   MOVEL     QQDCID        QQADOC
     C                   MOVE      '.ART'        QQADOC
     C                   MOVEL(P)  QQFOLD        QQAFLD
     C                   ENDIF
     C*** END   ADD *** 07/12/04 **************************************
      /Free
       tPath = '/' +%trim(strRoot) + %trim(tVolPt);
      /End-free
     C*** BEGIN ADD *** 07/21/04 **************************************
     C                   CALL      'SC871C'
     C                   PARM                    TPATH
     C*** END   ADD *** 07/21/04 **************************************
     C                   CAT(P)    '/':0         TVOLPT
     C                   CAT(P)    XXDCID:0      TVOLPT
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
     C*******CHANGE 05/23/04 ******************************
     C                   CAT(P)    '/':0         QQFOLD
     C                   CAT(P)    JOBNBR:0      QQFOLD
     C**** END CHG 05/23/04 ****************************
     C                   MOVEL     *BLANKS       QQRTCD
     C                   END
      *
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
     C*** ADD 08/15/02 ******
     C*====================================================================*
      **** DEBUG MODE (OR VIRTUAL OPTICAL PROCESS)
     C     DBGMOD        BEGSR
     C     *DTAARA       DEFINE                  VRTOPT            1
     C                   IN        *DTAARA                              83      ERROR IF 83
     C     *IN83         IFEQ      *OFF
     C*** FOUND THE DTAARA, SO CHECK FLAG TO SEE IF WE SHOULD USE VIRTUAL
     C**  OPTICAL PROCESSING
     C     VRTOPT        IFEQ      'Y'
     C** DO VRTOPT PROCESS
     C*** BEGIN ADD *** 07/12/04 **************************************
     C     Q1PRMS        IFLE      5
     C*** END   ADD *** 07/12/04 **************************************
     C                   CALL      'VRT871R'                            99
     C                   PARM                    QQKY             40
     C                   PARM                    QQSQ#             5
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM      XXOTYP        QQRTCD            1
     C*** BEGIN ADD *** 07/12/04 **************************************
     C                   ELSE
     C                   CALL      'VRT871R'                            99
     C                   PARM                    QQKY             40
     C                   PARM                    QQSQ#             5
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM      XXOTYP        QQRTCD            1
     C                   PARM                    QQADOC           12
     C                   PARM                    QQAFLD           80
     C                   ENDIF
     C*** END   ADD *** 07/12/04 **************************************
     C     *IN99         CABEQ     *ON           ENDDBG
     C*
     C*
     C                   GOTO      TERM
     C                   ELSE
     C                   GOTO      ENDDBG
     C                   ENDIF
     C                   ELSE
     C*** NO DATA AREA, SO NO VIRT OPTICAL PROCESSING DO NORMAL
     C                   GOTO      ENDDBG
     C                   ENDIF
     C*
     C     ENDDBG        ENDSR
     C*
     C*** END ADD 08/15/02 ******
