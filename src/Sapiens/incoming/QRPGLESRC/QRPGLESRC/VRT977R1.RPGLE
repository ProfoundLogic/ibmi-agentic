     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      *===============================================================*
      ** REVISIONS:
     H**
     H*                                                               *
     H* 12/05/15  C:33  Project: SY195                                *
     H*                 Remove QDLS hardcoding                        *
     H*                                                               *
     H* 04/15/14  C:300 Project: SY195                                *
     H*                 Remove SY195 Changes (11/02/10)               *
     H*                                                               *
     H* 09/16/13  C:33  Project: SY195                                *
     H*                 Allow for more than 5 positions for the root  *
     H**
     H* 09/05/13 C:062 Project ID132 82619                            *
     H*                CHECK SMOCP IF NOT HIT ON SMNOP                *
     H*                                                               *
     H* 01/12/11 C:062 Project ID114 75384                            *
     H*                USE SMNOP INSTEAD OF SMOCP                     *
    H*                                                                   *
    H* 11/02/10  C:183 Project: SY195                                    *
    H*                 Remove hard coding for QDLS. Call RTVDOCROOT to   *
    H*                 get the root folder name.                         *
     H**
     H****  02 - AOOPTICAL - DATACENTER PROCESSING TO BE ADDED FOR
     F***                    OPTICAL
     H**   10/10/06 -INCLUDE DATACENTER PREFIX PROCESSING
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
      *
      * 06/30/04 C:062 Project SY100
      *                Add Data Center Processing
      *
      *  05/17/04 C:128  Support: 42400                                 *
      *                  Added check to the name of the previous doc.   *
      *                  If new doc name is the same as the previous    *
      *                  name. Rename the new doc.
      *
     H**  C:02    120IAD   -  MODIFY TO GET THE SMOCP FILE TO RETRIEVE
     H**                  01/26/04    THE OPTICAL LIBRARY NAME
     H****  08/08/02 C02 - CHANGE TO USE THE ORIGINAL SUFFIX (.XXX) FROM THE
     H*                    DOCUMENT FOR EXECUTION IN VDARTS
      *
      ***   03/14/03 C:02   Changed the logical SMOLL to SMOLL1 removed
      *                     the system name from the key. The system
      *                     name is unused at this time.
      *
      *
      *
      ****>>>> RETRIEVE OBJECTS   FROM OPTICAL <<<<<******
      *
      *
      *
      *** Begin Change 08/16/02 ***************************************
     F******SMOLL   IF  E           K        DISK
     FSMOLL1    IF   E           K DISK
      *** End   Change 08/16/02 ***************************************
     F*
     F**** ADD 01/26/04 ***********************************************
     F*** BEGIN CHG *** 01/12/11 **************************************
     F**********SMOCP   IF  E                    DISK
     F**********  POC01                             KRENAMEPOC02
     FSMNOL     IF   E           K DISK
     F*** END   CHG *** 01/12/11 **************************************
     F**** END ADD 01/26/04 *******************************************
     F*** BEGIN ADD *** 09/05/13 **************************************
     FSMOCP     IF   E             DISK    Rename(poc01:poc02)
     F*** END   ADD *** 09/05/13 **************************************

     D******************  MS      1   1 49
     D*** BEGIN ADD *** 07/12/04 **************************************
     D OID             S              1    DIM(40)
     D*** END   ADD *** 07/12/04 **************************************
      *
     D*
     D*** CHANGE 1/26/04 *** GET LIBN FROM SMOCP INSTEAD
     D*** DEL       '/OPTMLB01'           C         OPTDIR
     D*** Begin Chg *** 07/09/04 **************************************
     D**********XXDCID      DS
     D**********                              1   1 TPRE
     D**********                              2   50TJOB#
     D****08/08/02 **ADD NXT LINE             6   80THHM
     D**********                              6   80TMSS
     D**********                              9   9 TPERD
     D****08/08/02 *** RMV                   10  120TMSS
     D**********                             10  12 TSFX
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
     D***  Begin Add  ***  12/05/15  ****
     DRtvDocRoot       pr                  extPgm('RTVDOCROOT')
     D inRoot                        10

     DstrRoot          s             10
     D***  End   Add  ***  12/05/15  ****
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
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

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
     C*** BEGIN ADD *** 01/12/11 **************************************
     C     NOKY          KLIST
     C                   KFLD                    OLOSYS
     C*** END   ADD *** 01/12/11 **************************************
      *
     C     OLKY          KLIST
     C                   KFLD                    QQKY
     C                   KFLD                    XXSEQ#
      *** Begin Change 08/16/02 ***************************************
     C******************** KFLD           QQSYSN
      *** Begin Change 08/16/02 ***************************************
     C                   KFLD                    QQOBJP
     C                   KFLD                    QQOID
     C*
      ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **
     C*
      * * * VERIFY THAT THE REQUESTED OBJ IS ON OPTICAL.
      * * * IF SO, GET THE NEEDED INFO.
      * * * PROCESS THE OPTICAL RETRIEVE. (WE WILL GET THE NAME OF THE DOC
      * * * TO BUILD, AND FOLDER FROM A CONTROL FILE.  IF AN ERROR OCCURS,
      * * * WE MUST RETURN AN NON-BLANK VALUE IN RTCD!!!
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKY             40
     C                   PARM                    QQSQ#             5
     C                   PARM                    QQSYSN           20
     C                   PARM                    QQOBJP           60
     C                   PARM                    QQOID            40
     C                   PARM                    QQOFLG            1
     C                   PARM                    QQRTCD            1
     C*** BEGIN ADD *** 07/12/04 **************************************
     C                   PARM                    QQAID            40
     C                   PARM                    QQAPTH           60
     C*** END   ADD *** 07/12/04 **************************************
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
     C                   MOVEL     *BLANKS       QQRTCD
     C*** BEGIN DEL *** 01/12/11 **************************************
     C*** 01/26/04 ** ADD *****************************************8
     C********** 1         CHAINSMOCP                64
     C********** *IN64     IFEQ *OFF
     C**********           MOVELOCOSYS    QQOSYS  1
     C**********           MOVELOCLIBN    QQLIBN 10
     C**********           ENDIF
     C*
     C********** '/'       CAT  QQLIBN:0  OPTDIR 11 P
     C*** 01/26/04 ** END ADD *************************************8
     C*** END   DEL *** 01/12/11 **************************************
     C*
     C                   TESTN                   QQSQ#                  67
     C     QQSQ#         IFNE      *BLANKS
     C*  DO SOMETHIN HERE WITH THE IN67???
     C                   MOVE      QQSQ#         XXSEQ#            5 0
     C                   ELSE
     C                   MOVE      *ZEROS        XXSEQ#
     C                   ENDIF
     C                   MOVE      *BLANKS       MSGDSC          256
     C*
      *************************************************************
     C*
     C*
      *** GET OBJ LOCATION RECORD
      *
     C     OLKY          CHAIN     POL01                              60
      *
      *** NO OBJ LOCATION RCD EXISTS... OBJECT NOT ON OPTICAL
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANK        QQOFLG            1
     C                   GOTO      TERM
     C                   END
     C*** BEGIN ADD *** 01/12/11 **************************************
     C     NOKY          CHAIN     PNO01                              64
     C     *IN64         IFEQ      *OFF
     C                   MOVEL     NOOSYS        QQOSYS            1
     C                   MOVEL     NOLIBN        QQLIBN           10
     C*** BEGIN ADD *** 09/05/13 **************************************
     C                   ELSE
     C     1             CHAIN     SMOCP                              64
     C     *IN64         IFEQ      *OFF
     C                   MOVEL     OCOSYS        QQOSYS
     C                   MOVEL     OCLIBN        QQLIBN
     C                   ENDIF
     C*** END   ADD *** 09/05/13 **************************************
     C                   ENDIF
     C*
     C     '/'           CAT(P)    QQLIBN:0      OPTDIR           11
     C*** END   ADD *** 01/12/11 **************************************
      *
     C                   MOVEL     'Y'           QQOFLG
      *
      *** END IN ERROR IF NOT A VALID OPTICAL RECORD.
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
     C                   MOVEL     OPTDIR        SPFSYS                         FROM VRTOPT
     C*****************    MOVEL'/QDLS'   SPFSYS           FROM OPTICAL
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
     C*** End   Del *** 07/09/04 **************************************
     C*** 08/08/02  DEL    MOVELDSTIME    THHM
     C                   MOVEL     '.'           TPERD
     C*** Begin Del *** 07/09/04 **************************************
     C**********           MOVE DSTIME    TMSS
     C*** End   Del *** 07/09/04 **************************************
     **** C:02 08/08/02 ************** ADD ************ ***********
     **  CHANGE PROGRAM TO USE ORIGINAL SUFFIX FROM DOC (TSFX)
     **
     C                   Z-ADD     *ZERO         G
     C     '.'           SCAN      QQOID:1       G                 2 0    43
     C     *IN43         IFEQ      *ON                                          FOUND
     C                   ADD       1             G
     C     3             SUBST     QQOID:G       TSFX                   44
     C     *IN44         IFEQ      *ON                                          ERROR
     C                   MOVEL     'TXT'         TSFX
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     'TXT'         TSFX
     C                   ENDIF
     **
     **** END ADD *************************************************
     C*
     C*
     C***  CREATE THE TO  PATH
     C*** Begin Chg *** 07/09/04 **************************************
     C********** '/'       CAT  'OPTTEMP' TVOLPT    P
     C                   MOVE      Q1JOBN        JOBNBR            6
     C*** BEGIN ADD *** 06/30/04 **************************************
     C                   CALL      'SA030R'
     C                   PARM      *BLANKS       ##PRFX            3
     C                   PARM      *BLANKS       ##PFLR           80
     C*
     C     ##PFLR        IFNE      *BLANKS
     C     '/'           CAT       'OPTTEMP'     OPTFLD            8
     C     '/'           CAT(P)    ##PFLR:0      TVOLPT
     C                   CAT       OPTFLD:0      TVOLPT
     C                   ELSE
     C*** END   ADD *** 06/30/04 **************************************
     C     '/'           CAT(P)    'OPTTEMP'     TVOLPT
     C*** BEGIN ADD *** 06/30/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/30/04 **************************************
     C                   CAT(P)    '/':0         TVOLPT
     C                   CAT(P)    JOBNBR:0      TVOLPT
     C*** End   Chg *** 07/09/04 **************************************
     C*** BEGIN ADD *** 07/12/04 **************************************
     C     Q1PRMS        IFGT      7
     C                   Z-ADD     *ZERO         G
     C     '.'           SCAN      QQOID:1       G                 2 0    43
     C     *IN43         IFEQ      *ON                                          FOUND
     C                   ADD       1             G
     C                   MOVEA     QQOID         OID
     C                   MOVEA     'ART'         OID(G)
     C                   MOVEA     OID           QQAID
     C                   ELSE
     C     QQOID         CAT(P)    '.ART':0      QQAID
     C                   ENDIF
     C                   MOVEL(P)  QQOBJP        QQAPTH
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
     C                   CALL      'IS977C1'
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
     C     '/'           CAT(P)    OLVOL1:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    OLOFLD:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    OLOSBD:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    OLDCID:0      SVOLPT
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
     C***************      CALL 'QHFCPYSF'PCPYSF           CPY DOC TO DASD
     C                   CALL      'VRTCPYC'
     C                   PARM                    SPATH
     C                   PARM                    TPATH
     C                   PARM      *BLANKS       RETCD
      *
      *
      *
     C*** CHECK ERROR STUFF ***
     C*
     C********   RTCD      IFNE 0
     C     RTCD          IFNE      *BLANK
     C     CONDTN        ANDNE     'CPF8A82'
     C*
     C                   MOVE      '1'           ERR               1
     C                   MOVEL     *BLANKS       QQOID
     C                   MOVEL     *BLANKS       QQOBJP
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
     C*** SET RETURN CODE HERE **********
     C*** DONT WAIT ON REPLY IN THIS CASE, WE WANT TO RETURN AN ERROR!
     C*
     C                   CALL      'EXCMSGC'     PLBRK
     C*
     C     QMRTN         IFEQ      '*'
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   END
     C*
     C                   ELSE
     C                   MOVEL(P)  XXDCID        QQOID
     C                   MOVEL(P)  'OPTTEMP'     QQOBJP
     C*** BEGIN ADD *** 06/30/04 **************************************
     C                   CAT       '/':0         QQOBJP
     C                   CAT       JOBNBR:0      QQOBJP
     C*** END   ADD *** 06/30/04 **************************************
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
********
Error** No Document Location can be established
