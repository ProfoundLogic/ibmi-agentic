     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**** USE CPY STREAM FILE THIS TIME
    H*********************************************************************
    H* Revisions:                                                        *
    H*                                                                   *
    H* 04/15/14  C:300 Project: SY195                                    *
    H*                 Remove SY195 Changes (11/02/10)                   *
    H*                                                                   *
    H* 11/02/10  C:183 Project: SY195                                    *
    H*                 Remove hard coding for QDLS. Call RTVDOCROOT to   *
    H*                 get the root folder name.                         *
    H*********************************************************************
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
      *** Beg add ***** 11/02/10 ***********************************************
     C***** beg del ****** 04/15/14 **********************************
     D***** RPGM            C                   CONST('RTVDOCROOT')
     C***** end del ****** 04/15/14 **********************************
      *** End add ***** 11/02/10 ***********************************************

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
     C     *ENTRY        PLIST                                                  "R"READ OR "W"-
     C                   PARM                    SPTH             63            FRM FLR/DOC   )
     C                   PARM                    TPTH             63            TO FLR/DOC    )
     C*
     C                   MOVE      '1'           TYPOFC
     C*
      *** Beg add ***** 11/02/10 ***********************************************
     C***** beg del ****** 04/15/14 **********************************
     C*****              CALL      RPGM
     C*****              PARM                    ROOT             10
     C***** end del ****** 04/15/14 **********************************
      *** End add ***** 11/02/10 ***********************************************
     C*
     C*
     C                   MOVE      *ZERO         RCLEN
     C                   MOVE      *ZERO         RTCD
     C*
     C                   EXSR      CPYSF
     C*
     C                   MOVE      '1'           *INLR
     C*
     C     CPYSF         BEGSR
     C                   MOVEL     '/QOPT'       SPFSYS
      *** Beg chg ***** 11/02/10 ***********************************************
     C*********            MOVEL'/QDLS'   TPFSYS
     C***** beg chg ****** 04/15/14 **********************************
     C*****     '/'           CAT       ROOT          TPFSYS
     C                   MOVEL     '/QDLS'       TPFSYS
     C***** end chg ****** 04/15/14 **********************************
      *** End chg ***** 11/02/10 ***********************************************
     C                   MOVEL     SPTH          SVOLPT
     C                   MOVEL     TPTH          TVOLPT
     C                   Z-ADD     80            SPTHL
     C                   Z-ADD     80            TPTHL
     C                   Z-ADD     272           RCLEN
     C                   CALL      'QHFCPYSF'    PCPYSF
      *
     C*** CHECK ERROR STUFF ***
     C*
     C     RTCD          IFNE      0
     C                   MOVE      '1'           ERR               1
     C                   END
     C     CPYEND        ENDSR
     C*
