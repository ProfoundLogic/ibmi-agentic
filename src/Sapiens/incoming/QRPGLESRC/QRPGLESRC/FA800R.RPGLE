     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - FA800R - UPDATE ALL WCMC EMPLOYER FILES                       A
     H*****************************************************************
     H*                                                               *
     H* This API is part of the the Tranfer process of moving         *
     H* an employer from one group to another.  Specifically this     *
     H* program updates all the files with the changed group and      *
     H* employer information for WCMC.  Additionally, the program     *
     H* writes a record to a history file.                            *
     H*                                                               *
     H*    CREATED: 05/26/99  C:112 Proj: 65WDC                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/05/01 C:15  Support: WC065/29279                           *
     H*                Changed pgm due to flaw in code which prevents *
     H*                any files from being updated.                  *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*  60         GENERAL USAGE (CHAIN)                             *
     H*                                                               *
     H*****************************************************************
      *
      * EMPLOYER MASTER FILE
     FFMEML     UF   E           K DISK
      *
     F*  Logical file definition for WCMS Employer Transfer File
     FFAETL     O  A E             DISK
      *

     D DBFDS           DS
     D  DBRRN                397    400B 0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
      * EMPLOYER MASTER FILE

     IPEM01
     I              EMCO#                       XXCO#
     I              EMSUB#                      XXSUB#
     I              EMFND                       XXFND
     I              EMEMP#                      XXEMP#
     I              EMDIV                       XXDIV
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FGRP            3
     C                   PARM                    @@FEMP            9
     C                   PARM                    @@FDIV            5
     C                   PARM                    @@TGRP            3
     C                   PARM                    @@TEMP            9
     C                   PARM                    @@TDIV            5
      *
      *
      *****************************************************************
      *  KEYS
      *****************************************************************
      *
      * Key used to retrieve 'From' data from files
      *
     C     FRKY          KLIST
     C                   KFLD                    XXCO#
     C*** Begin dlt *** 12/05/01 **************************************
     C*******************  KFLD           XXSUB#
     C*******************  KFLD           XXFND
     C*******************  KFLD           XXEMP#
     C*******************  KFLD           XXDIV
     C*** End dlt ***** 12/05/01 **************************************
     C*** Begin add *** 12/05/01 **************************************
     C                   KFLD                    XXFSUB            5 0
     C                   KFLD                    XXFFND            3 0
     C                   KFLD                    XXFEMP            9 0
     C                   KFLD                    XXFDIV            5 0
     C*** End add ***** 12/05/01 **************************************
      *
      *****************************************************************
      *  KEYS
      *****************************************************************
      *
      * PLIST
      *****************************************************************
      *
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
      *****************************************************************
      *  MAIN
      *****************************************************************
      *
      * call program to translate WCMS CO#/FND to WCMC CO#/SUB#/FND
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVE      @@CO#         ##CO#
     C                   MOVE      @@FGRP        ##WCF#
     C                   EXSR      XLATE
     C*
     C     ##RTCD        IFNE      'F'
     C                   MOVE      ##SUB#        ETOSUB
     C                   MOVE      ##FND         ETOFND
      *
     C                   MOVE      @@TGRP        ##WCF#
     C                   EXSR      XLATE
     C*
     C     ##RTCD        IFNE      'F'
     C                   MOVE      ##SUB#        ETSUB#
     C                   MOVE      ##FND         ETFND
     C*
     C* Write record to WCMS Employer Transfer File
     C* used to maintain a history
     C*
     C                   MOVE      @@CO#         ETCO#
     C                   MOVE      @@TEMP        ETEMP#
     C                   MOVE      @@TDIV        ETDIV
     C                   MOVE      *BLANKS       ETFYR
     C                   MOVE      *BLANKS       ETCASE
     C                   MOVE      @@FEMP        ETOEMP
     C                   MOVE      @@FDIV        ETODIV
     C                   MOVE      *BLANKS       ETOFYR
     C                   MOVE      *BLANKS       ETOCAS
     C                   MOVE      Q1USER        ETUSER
     C                   MOVE      Q1PGM         ETPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ETDT
     C                   MOVE      DSTIME        ETTIME
     C                   MOVE      Q1USER        ETIUSR
     C                   MOVE      Q1PGM         ETIPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ETIDT
     C                   MOVE      DSTIME        ETITIM
     C*
     C                   WRITE     PET01
      *
     C                   EXSR      LOADFR
     C*
     C     FRKY          CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PEM01
     C     FRKY          READE     PEM01                                  60
     C                   ENDDO
     C*
     C                   ENDIF
     C                   ENDIF
     C*
      *
     C                   MOVE      '1'           *INLR
      *****************************************************************
      *  LOADFR
      *****************************************************************
      *
     C     LOADFR        BEGSR
      *
     C*** Begin dlt *** 12/05/01 **************************************
     C*******************  MOVE @@CO#     ##CO#
     C*******************  MOVE @@FGRP    ##WCF#
     C*******************  EXSR XLATE
     C*** End dlt ***** 12/05/01 **************************************
     C*
     C                   MOVE      @@CO#         XXCO#
     C*** Begin dlt *** 12/05/01 **************************************
     C*******************  MOVE ##SUB#    XXSUB#
     C*******************  MOVE ##FND     XXFND
     C*******************  MOVE @@FEMP    XXEMP#
     C*******************  MOVE @@FDIV    XXDIV
     C*** End dlt ***** 12/05/01 **************************************
     C*** Begin add *** 12/05/01 **************************************
     C                   MOVE      ETOSUB        XXFSUB
     C                   MOVE      ETOFND        XXFFND
     C                   MOVE      @@FEMP        XXFEMP
     C                   MOVE      @@FDIV        XXFDIV
     C*** End add ***** 12/05/01 **************************************
      *
     C                   ENDSR
      *
      *****************************************************************
      *  LOADTO
      *****************************************************************
      *
     C     LOADTO        BEGSR
      *
     C*** Begin dlt *** 12/05/01 **************************************
     C*******************  MOVE @@CO#     ##CO#
     C*******************  MOVE @@TGRP    ##WCF#
     C*******************  EXSR XLATE
     C*** End dlt ***** 12/05/01 **************************************
     C*
     C                   MOVE      @@CO#         XXCO#
     C*** Begin dlt *** 12/05/01 **************************************
     C*******************  MOVE ##SUB#    XXSUB#
     C*******************  MOVE ##FND     XXFND
     C*** End dlt ***** 12/05/01 **************************************
     C*** Begin add *** 12/05/01 **************************************
     C                   MOVE      ETSUB#        XXSUB#
     C                   MOVE      ETFND         XXFND
     C*** End add ***** 12/05/01 **************************************
     C                   MOVE      @@TEMP        XXEMP#
     C                   MOVE      @@TDIV        XXDIV
      *
     C                   ENDSR
      *****************************************************************
      *  XLATE
      *****************************************************************
      *
     C     XLATE         BEGSR
     C*
     C                   CALL      'FA700R'                             99
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##WCF#            3
     C                   PARM      *ZEROS        ##SUB#            5
     C                   PARM      *ZEROS        ##FND             3
     C*
     C                   ENDSR
