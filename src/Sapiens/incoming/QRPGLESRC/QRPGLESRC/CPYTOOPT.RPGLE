     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**   REVISIONS:
     H****  02 - AOOPTICAL - DATACENTER PROCESSING TO BE ADDED FOR

     D***                    OPTICAL
     H**   10/10/06 -INCLUDE DATACENTER PREFIX PROCESSING
     H*                                                               *
     D*              02   11/01/04 - ALLOW CREATION OF 2 COPIES OF OBJECTS
     D*                              1 ON OPTICAL ONE ON IFS
     D*                              IF DUPOPT DATAARA (EXISTS) AND HAS A
     D*                              "Y", THEN BYPASS CHECK FOR VRTOPT UNTIL
     D*                              AFTER THE QOPT PROCESS IS COMPLETE.
     D*
     H*
     H**** USE CPY STREAM FILE THIS TIME
     F*
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
     D  RCLEN                  1      4B 0
     D  RTCD                   5      8B 0
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
     C*** ADD 11/01/04 ******************************************************
     C*** DON'T PERFORM THE VRTOPT CHECK HERE IF THE DUPOPT IS A Y UNTIL
     C***   AFTER THE NORMAL PROCESS HAS OCCURED.
     C***
     C     *DTAARA       DEFINE                  DUPOPT            1
     C                   IN        DUPOPT                               87
     C     *IN87         IFEQ      *ON                                             NO DTAARA
     C     DUPOPT        ORNE      'Y'                                             NOT DUPOPT
     C*** END ADD 11/01/04 **************************************************
     C***** ADD 08/15/02 ***
     C                   EXSR      DBGMOD
     C***** END ADD 08/15/02 ***
     C*** ADD 11/01/04 ******************************************************
     C                   ENDIF
     C*** END ADD 11/01/04 **************************************************
     C*
     C                   MOVE      '0'           TYPOFC                         NO REPLACE
     C*
     C*
     C*** BEGIN ADD *** 10/10/06 **************************************
     C                   CALL      'SA030R'
     C                   PARM      *BLANKS       ##SUFX            3
     C                   PARM      *BLANKS       ##PFLR           80
     C*** END   ADD *** 10/10/06 **************************************
     C*
     C*
     C                   MOVE      *ZERO         RCLEN
     C                   MOVE      *ZERO         RTCD
     C*
     C                   EXSR      CPYSF
     C*** ADD 11/01/04 ******************************************************
     C*** IF DUPOPT THEN PERFORM THE VRTOPT CHECK AFTER NORMAL PROCESSING
     C***
     C                   IN        DUPOPT                               87
     C     *IN87         IFEQ      *OFF                                           DTAARA EXISTS
     C     DUPOPT        ANDEQ     'Y'                                            DUPOPT IS 'Y'
     C                   EXSR      DBGMOD
     C                   ENDIF
     C*** END ADD 11/01/04 **************************************************
     C*
     C                   RETURN
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
      *====================================================================*
     C     CPYSF         BEGSR
     C                   MOVEL     '/QDLS'       SPFSYS
     C                   MOVEL     '/QOPT'       TPFSYS
     C                   MOVEL     SPTH          SVOLPT
     C                   MOVEL     TPTH          TVOLPT
     C                   Z-ADD     80            SPTHL
     C                   Z-ADD     80            TPTHL
     ******* ADD **** 10/10/06 *******************************************
     **  IF DATACENTER PROCESSING, WE SHOULD PREFIX THE PATHS
     **
     C     ##PFLR        IFNE      *BLANKS
     C     '/QDLS/'      CAT(P)    ##PFLR:0      @@WK3            80
     C                   CAT(P)    SVOLPT:0      @@WK3
     C                   MOVEL     @@WK3         SPATH
     **
     C                   ENDIF
     **
     ** END  ADD **** 10/10/06 *******************************************
     C                   Z-ADD     272           RCLEN
     C                   CALL      'QHFCPYSF'    PCPYSF
      *
     C*** CHECK ERROR STUFF ***
     C*
     C     RTCD          IFNE      0
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
     C*** ADD 08/15/02 ******
      *====================================================================*
      **** DEBUG MODE (OR VIRTUAL OPTICAL PROCESS)
     C     DBGMOD        BEGSR
     C     *DTAARA       DEFINE                  VRTOPT            1
     C**** CHANGE 11/01/04 *********************************************
     C*** ADDED ANOTHER DTAARA TO PGM, MUST 'NAME' THE "IN" OPERATION
     C***** DEL 11/01/04   IN   *NAMVAR                83  ERROR IF 83
     C                   IN        VRTOPT                               83      ERROR IF 83
     C**** END CHANGE 11/01/04 *********************************************
     C     *IN83         IFEQ      *OFF
     C*** FOUND THE DTAARA, SO CHECK FLAG TO SEE IF WE SHOULD USE VIRTUAL
     C**  OPTICAL PROCESSING
     C     VRTOPT        IFEQ      'Y'
     C** DO VRTOPT PROCESS
     C                   CALL      'VRTTOOPT'                           99
     C                   PARM                    SPTH             63
     C                   PARM                    TPTH             63
     C                   PARM      *BLANK        MSGDSC          256
     C                   PARM      *BLANK        QQRTCD            1
     C     *IN99         CABEQ     *ON           ENDDBG
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
