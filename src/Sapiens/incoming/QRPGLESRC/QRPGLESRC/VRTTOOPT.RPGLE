     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*  REVISIONS:                                                   *
    H*********************************************************************
    H* Revisions:                                                        *
    H*                                                                   *
    H* 11/02/10  C:183 Project: SY195                                    *
    H*                 Remove hard coding for QDLS. Call RTVDOCROOT to   *
    H*                 get the root folder name.                         *
    H*                                                                   *
     H****  02 - AOOPTICAL - DATACENTER PROCESSING TO BE ADDED FOR
     F***                    OPTICAL
     H**   10/10/06 -INCLUDE DATACENTER PREFIX PROCESSING
     H*                                                               *
     H*
     F**** USE CPY STREAM FILE THIS TIME
     F*
     H**  C:02    120IAD   -  MODIFY TO GET THE SMOCP FILE TO RETRIEVE
     H**                  01/26/04    THE OPTICAL LIBRARY NAME
     F**** ADD 01/26/04 ***********************************************
     FSMOCP     IF   E             DISK
     F                                     RENAME(POC01:POC02)
     F**** END ADD 01/26/04 *******************************************

     D*** CHANGE 1/26/04 *** GET LIBN FROM SMOCP INSTEAD
     D*** DEL       '/OPTMLB01'           C         OPTDIR
     D SPATH           DS
     D  SPFSYS                 1      5
     D  SVOLPT                 6     80
     D SPTHLN          DS
     D  SPTHL                  1      4B 0
     D*
     D TPATH           DS
     D  TPFSYS                 1      9
     D************************                1   5 TPFSYS
     D****************************            6  80 TVOLPT
     D  TVOLPT                10     80
     D TPTHLN          DS
     D  TPTHL                  1      4B 0
     D*
     D CPYINF          DS
     D  TYPOFC                 1      1
     D  CPYFIL                 2      6
     D*
      *** Beg add ***** 11/02/10 ***********************************************
     D RPGM            C                   CONST('RTVDOCROOT')
      *** End add ***** 11/02/10 ***********************************************
      *-------------------------------------------*
      *    RETCD HAS THE FOLLOWING INFO           *
      *-------------------------------------------*
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
      *-------------------------------------------*
      *
     D RETCD           DS
     D****************************        B   1   40RCLEN
     D***************************         B   5   80RTCD
     D  RTCD                   1      8
     D  CONDTN                 9     15
     D  RSV                   16     16
     D  MSG                   17    272

     I*

     C     PCPYSF        PLIST
     C                   PARM                    SPATH            80
     C                   PARM                    SPTHL
     C                   PARM                    CPYINF
     C                   PARM                    TPATH            80
     C                   PARM                    TPTHL
     C                   PARM                    RETCD
     C*
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
     C     *ENTRY        PLIST                                                  "R"READ OR "W"-
     C                   PARM                    SPTH             63            FRM FLR/DOC   )
     C                   PARM                    TPTH             63            TO FLR/DOC    )
     C                   PARM                    MSGDSC          256            ERR MSG DSC   )
     C                   PARM                    QQRTCD            1            RTN CODE
     C*
     C                   MOVE      '0'           TYPOFC                         NO REPLACE
      *** Beg add ***** 11/02/10 ***********************************************
     C                   CALL      RPGM
     C                   PARM                    ROOT             10
      *** End add ***** 11/02/10 ***********************************************
     C*
     C*** 01/26/04 ** ADD *****************************************8
     C     1             CHAIN     SMOCP                              64
     C     *IN64         IFEQ      *OFF
     C                   MOVEL     OCOSYS        QQOSYS            1
     C                   MOVEL     OCLIBN        QQLIBN           10
     C                   ENDIF
     C*
     C     '/'           CAT(P)    QQLIBN:0      OPTDIR           11
     C*** 01/26/04 ** END ADD *************************************8
     C*
     C*** BEGIN ADD *** 10/10/06 **************************************
     C                   CALL      'SA030R'
     C                   PARM      *BLANKS       ##SUFX            3
     C                   PARM      *BLANKS       ##PFLR           80
     C*** END   ADD *** 10/10/06 **************************************
     C*
     C                   MOVE      *ZERO         RCLEN            15 5
     C***********          MOVE *ZERO     RTCD
     C                   MOVE      *BLANKS       RTCD
     C*
     C                   EXSR      CPYSF
     C*
     C                   RETURN
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
      *====================================================================*
     C     CPYSF         BEGSR
      *** Beg chg ***** 11/02/10 ***********************************************
     C*******              MOVEL'/QDLS'   SPFSYS
     C     '/'           CAT       ROOT          SPFSYS
      *** End chg ***** 11/02/10 ***********************************************
     C******               MOVEL'/QDLS'   TPFSYS
     C                   MOVEL     OPTDIR        TPFSYS
     C                   MOVEL     SPTH          SVOLPT
     C                   MOVEL     TPTH          TVOLPT
     C                   Z-ADD     80            SPTHL
     C                   Z-ADD     80            TPTHL
     C                   Z-ADD     272           RCLEN
     ******* ADD **** 10/10/06 *******************************************
     **  IF DATACENTER PROCESSING, WE SHOULD PREFIX THE PATHS
     **
     C     ##PFLR        IFNE      *BLANKS
      *** Beg chg ***** 11/02/10 ***********************************************
     C******     '/QDLS/'  CAT  ##PFLR:0  @@WK3  80 P
     C     '/'           CAT(P)    ROOT          RPATH            12            Root path
     C     RPATH         CAT(P)    '/':0         RPATH
     C     RPATH         CAT(P)    ##PFLR:0      @@WK3            80
      *** End chg ***** 11/02/10 ***********************************************
     C                   CAT(P)    SVOLPT:0      @@WK3
     C                   MOVEL     @@WK3         SPATH
     **
     C     '/'           CAT(P)    ##PFLR:0      @@WK3
     C                   CAT(P)    OPTDIR:0      @@WK3
     C                   CAT(P)    TVOLPT:0      @@WK3
     C                   MOVEL     @@WK3         TPATH
     C                   ENDIF
     **
     ** END  ADD **** 10/10/06 *******************************************
     C*******8             CALL 'QHFCPYSF'PCPYSF
     C                   CALL      'VRTCPYC'
     C                   PARM                    SPATH
     C                   PARM                    TPATH
     C                   PARM      *BLANKS       RETCD
      *
     C*** CHECK ERROR STUFF ***
     C*
     C     RTCD          IFNE      *BLANK
     C                   MOVE      '1'           ERR               1
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
     C*** 9/19/00 *************************************
      * * * ERROR WITH OFFICE NOT ABLE TO CONVERT DOC
     C     CONDTN        IFEQ      'CPF1F71'
     C                   MOVEL     MSG           XX17A            17
     C                   MOVE      XX17A         XX7A              7
     C     XX7A          IFEQ      'CPF8A37 '
     C                   MOVEL     'CPF8A37'     CONDTN
     C                   MOVE      '5'           QQRTCD
     C                   END
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
     C                   END
     C     CPYEND        ENDSR
     C*
