     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      * *****************************************************************
      /TITLE FA707R - API TO CREATE BILL ENTRY HEADINGS
      * *****************************************************************
      *  Description:                                                   *
      *
      *  THIS API WILL CREATE A TWO LINE HEADING THAT WILL INCLUDE
      *  REMIT PROVIDER AND PHONE NUMBER AND SERVICING PROVIDER AND
      *  PHONE NUMBER.
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SEND   :  FAEOP RELATIVE RECORD NUMBER
      *  RECEIVE:  TWO 78 CHAR LONG FIELDS FOR HEADING
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  THIS IS WHAT THE HEADING WILL LOOK LIKE:
      *
      *  Remit to Provider:  XXXXXXXX - XXX XXXXXXXXXXXXXXXXXXXX (XXX) XXX - XXX
      *  Servicing Provider: XXXXXXXX - XXX XXXXXXXXXXXXXXXXXXXX (XXX) XXX - XXX
      *
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *                                                                 *
      *  Called From:                                                   *
      *    WORK WITH PROVIDER BILLS SEQUENCE                            *
      *                                                                 *
      *  Calls To:                                                      *
      *    None                                                         *
      *                                                                 *
      *  Created:  05/08/98  C:56  Project:  MC152                      *
      *                                                                 *
      * *****************************************************************
      *  Revisions:                                                     *
      *                                                                 *
      *  07/19/99  C:62  PROJECT: MC208
      *                  ADDED ADJUDICATION STATE AND GEO ZIP TO LINE.
      *                                                                 *
      *  08/03/98  C:94  PROJECT: MC152
      *                  DELETED SERVICING PROVIDER LINE.
      *
      *                                                                 *
      * *****************************************************************
      *  Indicators:                                                    *
      *                                                                 *
      *    99 - General File Access                                     *
      * *****************************************************************
      *
     FFAEOP     IF   E             DISK
     FFMPDL     IF   E           K DISK

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** BEG CHG *** 08/03/98 *************************************
     D*** BEGIN CHG *** 07/19/99 *************************************
     D**********  DS
     D**********                              1  78 HDL1
     D**********                              1  10 TITL1
     D**********                             13  21 FEIN1
     D**********                             22  24 DSH1A
     D**********                             25  27 MOD1
     D**********                             29  52 NAME1
     D**********                             54  55 OPNP1
     D**********                             56  58 AREA1
     D**********                             59  59 CLOP1
     D**********                             61  63 EXCH1
     D**********                             64  64 DSH1B
     D**********                             65  68 PHN#1
     D                 DS
     D  HDL1                   1     78
     D  TITL1                  1     10
     D  FEIN1                 11     19
     D  DSH1A                 20     20
     D  MOD1                  21     23
     D  NAME1                 25     42
     D  OPNP1                 44     44
     D  AREA1                 45     47
     D  CLOP1                 48     48
     D  EXCH1                 50     52
     D  DSH1B                 53     53
     D  PHN#1                 54     57
     D  TITL2                 59     63
     D  AST                   64     66
     D  TITL3                 67     75
     D  ZIPA                  76     78
     D*** END   CHG *** 07/19/99 *************************************
     D*******     DS
     D*******                                 1  78 HDL1
     D*******                                 1  19 TITL1
     D*******                                22  30 FEIN1
     D*******                                31  33 DSH1A
     D*******                                34  36 MOD1
     D*******                                38  61 NAME1
     D*******                                63  64 OPNP1
     D*******                                65  67 AREA1
     D*******                                68  68 CLOP1
     D*******                                70  72 EXCH1
     D*******                                73  73 DSH1B
     D*******                                74  77 PHN#1
     D*** END CHG *** 08/03/98 *************************************
     D*** BEG DEL *** 08/03/98 **************************************
     D***         DS
     D***                                     1  78 HDL2
     D***                                     1  19 TITL2
     D***                                    22  30 FEIN2
     D***                                    31  33 DSH2A
     D***                                    34  36 MOD2
     D***                                    38  61 NAME2
     D***                                    63  64 OPNP2
     D***                                    65  67 AREA2
     D***                                    68  68 CLOP2
     D***                                    70  72 EXCH2
     D***                                    73  73 DSH2B
     D***                                    74  77 PHN#2
     D*** END DEL *** 08/03/98 **************************************
     D*
     D*** BEG CHG *** 08/03/98 ***************************************
     D REMIT           C                   CONST('Provider:')
     D***           'Remit to Provider:'  C         REMIT
     D*** END CHG *** 08/03/98 ***************************************
     D***           'Servicing Provider:' C         SERV   DEL 08/03/98
     D DASH            C                   CONST(' - ')
     D SDASH           C                   CONST('-')
     D*** BEGIN CHG *** 07/19/99 **************************************
     D**********    ' ('                  C         OPENP
     D OPENP           C                   CONST('(')
     D*** END   CHG *** 07/19/99 **************************************
     D CLOSEP          C                   CONST(')')
     D*** BEGIN ADD *** 07/19/99 **************************************
     D GEOZIP          C                   CONST('Geo Zip: ')

     I*** ENDIN ADD *** 07/19/99 **************************************

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      HDL1          @@L1             78
     C***                  PARM HDL2      @@L2   78        DEL 08/03/98
     C*
     C**         DTMDYY    PLIST
     C**                   PARM           SRDTI   80
     C**                   PARM '*CYMD'   SRFFMT  8
     C**                   PARM *BLANKS   SRTFMT  8
     C**                   PARM           SRDTO  10
     C**                   PARM           SRERR   1
     C*
     C**  KEY  LIST FOR PROVIDER MASTER FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C*
     C                   CLEAR                   HDL1
     C***                  CLEARHDL2                       DEL 08/03/98
     C*
     C                   MOVEL(P)  REMIT         TITL1
     C***                  MOVELSERV      TITL2     P      DEL 08/03/98
     C*** BEGIN CHG *** 07/19/99 **************************************
     C**********           MOVELDASH      DSH1A
     C                   MOVEL     SDASH         DSH1A
     C*** END   CHG *** 07/19/99 **************************************
     C                   MOVEL     SDASH         DSH1B
     C***                  MOVELDASH      DSH2A            DEL 08/03/98
     C***                  MOVELSDASH     DSH2B            DEL 08/03/98
     C                   MOVEL     OPENP         OPNP1
     C***                  MOVELOPENP     OPNP2            DEL 08/03/98
     C                   MOVEL     CLOSEP        CLOP1
     C***                  MOVELCLOSEP    CLOP2            DEL 08/03/98
     C*
     C     @@RRN         CHAIN     PEO01                              99
     C                   MOVE(P)   EOFEIN        FEIN1
     C                   MOVE(P)   EOMOD         MOD1
     C***                  MOVE EOSFEI    FEIN2     P      DEL 08/03/98
     C***                  MOVE EOSMOD    MOD2      P      DEL 08/03/98
     C*** BEGIN ADD *** 07/19/99 **************************************
     C                   MOVEL(P)  'AST: '       TITL2
     C                   MOVEL(P)  EOAST         AST
     C                   MOVEL(P)  GEOZIP        TITL3
     C                   MOVEL(P)  EOZIPA        ZIPA
     C*** END   ADD *** 07/19/99 **************************************
     C*
     C                   MOVE      FEIN1         KKFEIN
     C                   MOVE      MOD1          KKMOD
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     PDNAME        NAME1
     C     PDAREA        IFEQ      *ZEROS
     C     PDPHN#        ANDEQ     *ZEROS
     C                   MOVE      *BLANKS       OPNP1
     C                   MOVE      *BLANKS       AREA1
     C                   MOVE      *BLANKS       CLOP1
     C                   MOVE      *BLANKS       EXCH1
     C                   MOVE      *BLANKS       DSH1B
     C                   MOVE      *BLANKS       PHN#1
     C                   ELSE
     C                   MOVE      PDAREA        AREA1
     C                   MOVEL     PDPHN#        EXCH1
     C                   MOVE      PDPHN#        PHN#1
     C                   ENDIF                                                  PDAREA IFEQ ZEROS
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*
     C*** BEG DEL *** 08/03/98 **************************************
     C***                  MOVE FEIN2     KKFEIN
     C***                  MOVE MOD2      KKMOD
     C***        PDKY      CHAINPPD01                99
     C***        *IN99     IFEQ *OFF
     C***                  MOVELPDNAME    NAME2
     C***        PDAREA    IFEQ *ZEROS
     C***        PDPHN#    ANDEQ*ZEROS
     C***                  MOVE *BLANKS   OPNP2
     C***                  MOVE *BLANKS   AREA2
     C***                  MOVE *BLANKS   CLOP2
     C***                  MOVE *BLANKS   EXCH2
     C***                  MOVE *BLANKS   DSH2B
     C***                  MOVE *BLANKS   PHN#2
     C***                  ELSE
     C***                  MOVE PDAREA    AREA2
     C***                  MOVELPDPHN#    EXCH2
     C***                  MOVE PDPHN#    PHN#2
     C***                  ENDIF                           PDAREA IFEQ ZEROS
     C***                  ENDIF                           *IN99 IFEQ *OFF
     C*** END DEL *** 08/03/98 **************************************
     C*
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
