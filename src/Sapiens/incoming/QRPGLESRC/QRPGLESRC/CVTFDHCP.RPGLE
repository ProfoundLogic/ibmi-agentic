     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTFDHCP - LOAD FDHCP FROM FDCAP
     H**************************************************************************
     H* THIS PROGRAM WILL LOAD FDHCP RECORDS FROM RECORDS IN FDCAP.  THIS PGM  *
     H* IS NEEDED BECAUSE ORIGINALLY WE WERE ONLY CREATING RECORDS IN FDHCP    *
     H* FOR HOSPITAL BILLS(WHEN WE NEEDED).  CURRENTLY SOTFWARE CREATES AN     *
     H* FDHCP RECORD EVERYTIME IT CREATES AN FDCAP RECORD.  CERTAIN PROGRAMS   *
     H* (I.E. FLORIDA TAPE SUBMISSION) WILL TRY TO USE FDHCP RECORDS.  THESE   *
     H* RECORDS WOULD NOT EXIST, WHEN A CLIENT IS UPDATED FROM FSMS TO WCMC.   *
     H* THIS PROGRAM WILL CREATE THESE RECORDS AND KEEP THE SYSTEM IN SYNC.    *
     H**************************************************************************
     F*
     FFDCAP     IF   E           K DISK
     FFDHCL     UF A E           K DISK
     FFTABL     IF   E           K DISK
     FFMPDL     IF   E           K DISK

     D*
     D* DATA STRUCTURE TO LOAD FIELD HC10A1
     D*
     D                 DS
     D  XX10A1                 1     10
     D  X3SPLT                 1      3
     D  X510A1                 5      6
     D*
     D* DATE AND TIME DATA STRUCTURE
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C* KEY LIST FOR THE DETAIL CHARGE AUXILIARY FILE
     C*
     C*
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
     C     HCKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CADSEQ
     C*
     C* PROVIDER MASTER FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C*
     C* PLACE OF SERVICE TABLE FILE KEY LIST
     C*
     C     PSKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    TBKEY
     C*
     C* INITIALIZATION SECTION
     C*
     C     *LIKE         DEFINE    PSKEY         TBKEY
     C                   TIME                    DSDTTM
     C*
     C* READ THE DETAIL CHARGE FILE
     C*
     C                   READ      PCA01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C* CHAIN TO THE DETAIL CHARGE AUXILIARY FILE TO SEE IF RECORD EXISTS
     C*
     C     HCKY          CHAIN     PHC01                              60
     C     *IN60         IFEQ      '1'
     C*
     C* IF NO MATCH IS FOUND IN THE DETAIL CHARGE AUXILIARY FILE - CREATE RECORD
     C*
     C                   CLEAR                   PHC01
     C                   Z-ADD     CACO#         HCCO#
     C                   Z-ADD     CASUB#        HCSUB#
     C                   Z-ADD     CAFND         HCFND
     C                   Z-ADD     CAFYR         HCFYR
     C                   Z-ADD     CACASE        HCCASE
     C                   Z-ADD     CADSEQ        HCDSEQ
     C                   MOVE      *BLANKS       HCDESC
     C                   MOVE      *BLANK        HCSTAT
     C                   MOVEL     'PDSPGMR'     HCUSER
     C                   MOVEL     'CVTFDHCP'    HCPGM
     C                   Z-ADD     DSTIME        HCTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HCDT
     C*
     C                   MOVEL     'PDSPGMR'     HCIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HCIDT
     C*
     C                   MOVE      *BLANKS       HCNDC
     C                   Z-ADD     *ZEROS        HCRXFD
     C                   MOVE      *BLANKS       HCRX#
     C                   Z-ADD     *ZEROS        HCRXMQ
     C                   MOVE      *BLANKS       HCRXMM
     C                   Z-ADD     *ZEROS        HCRXDY
     C                   MOVE      *BLANKS       HC1A1
     C                   MOVE      *BLANKS       HC1A2
     C                   MOVE      *BLANKS       HC1A3
     C                   MOVE      *BLANKS       HC1A4
     C                   MOVE      *BLANKS       HC1A5
     C                   Z-ADD     *ZEROS        HC6N2
     C                   Z-ADD     *ZEROS        HC6N3
     C                   Z-ADD     *ZEROS        HC6N4
     C                   Z-ADD     *ZEROS        HC6N5
     C                   Z-ADD     *ZEROS        HC52N1
     C                   Z-ADD     *ZEROS        HC52N2
     C                   Z-ADD     *ZEROS        HC52N3
     C                   Z-ADD     *ZEROS        HC52N4
     C                   Z-ADD     *ZEROS        HC52N5
     C                   Z-ADD     *ZEROS        HC92N1
     C                   Z-ADD     *ZEROS        HC92N2
     C                   Z-ADD     *ZEROS        HC92N3
     C                   Z-ADD     *ZEROS        HC92N4
     C                   Z-ADD     *ZEROS        HC92N5
     C                   MOVE      *BLANKS       HC10A2
     C                   MOVE      *BLANKS       HC10A3
     C                   MOVE      *BLANKS       HC10A4
     C                   MOVE      *BLANKS       HC10A5
     C                   MOVE      *BLANKS       HC40A1
     C                   MOVE      *BLANKS       HCEO01
     C                   MOVE      *BLANKS       HCEO02
     C                   MOVE      *BLANKS       HCEO03
     C                   MOVE      *BLANKS       HCEO04
     C                   MOVE      *BLANKS       HCEO05
     C                   MOVE      *BLANKS       HCEO06
     C                   MOVE      *BLANKS       HCEO07
     C                   MOVE      *BLANKS       HCEO08
     C                   MOVE      *BLANKS       HCEO09
     C                   MOVE      *BLANKS       HCEO10
     C                   MOVE      *BLANKS       HCBRNC
     C*
     C* GET THE TWO DIGIT PLACE OF SERVICE CODE FROM THE POS TABLE FILE USING
     C* THE ONE DIGIT PLACE OF SERVICE CODE IN THE DETAIL CHARGE FILE(CAIPOP)
     C*
     C                   MOVE      *BLANKS       TBKEY
     C                   MOVEL     CAIPOP        TBKEY
     C     PSKY          CHAIN     PPS01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* IF WE GET A HIT IN THE POS TABLE FILE - LOAD THE TWO DIGIT POS
     C*
     C                   MOVE      PSPOS         HCPOS
     C*
     C                   ELSE                                                   ELSE *IN60 EQ '0'
     C*
     C* IF WE DO NOT GET A HIT IN THE POS FILE - LOAD *BLANKS TO THE TWO DIGIT
     C* POS
     C*
     C                   MOVE      *BLANKS       HCPOS
     C*
     C                   END                                                    END *IN60 EQ '0'
     C*
     C* GET THE PROVDER SPECIALTY CODE, ZIP CODE - FROM THE PROVIDER MASTER FILE
     C*
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* IF WE GET A HIT IN THE PROVIDER MASTER FILE - LOAD THE PROVIDER SPLTY
     C* CODE, ZIP CODE
     C*
     C                   MOVEL     PDSPCD        X3SPLT
     C                   MOVE      'FS'          X510A1
     C                   MOVE      XX10A1        HC10A1
     C                   MOVEL     PDZIP         HC6N1
     C*
     C                   ELSE                                                   ELSE *IN60 EQ '0'
     C*
     C* IF WE DO NOT GET A HIT IN THE PROVIDER MASTER FILE - MOVE BLANKS TO THE
     C* PROVIDER SPLTY CODE AND ZEROS TO THE PROVIDER ZIP
     C*
     C                   MOVE      *BLANKS       X3SPLT
     C                   MOVE      'FS'          X510A1
     C                   MOVE      XX10A1        HC10A1
     C                   Z-ADD     *ZEROS        HC6N1
     C*
     C                   END                                                    END *IN60 EQ '0'
     C*
     C* WRITE THE DETAIL CHARGE AUXILIARY FILE RECORD
     C*
     C                   WRITE     PHC01
     C*
     C* ELSE WE GET A HIT IN THE DETAIL CHARGE AUXILIARY FILE - IF WE GET A HIT
     C* RELEASE RECORD AND READ NEXT DETAIL CHARGE FILE RECORD
     C*
     C                   ELSE                                                   ELSE *IN60 EQ '1'
     C*
     C                   EXCEPT    PHC01E
     C*
     C                   END                                                    END *IN60 EQ '1'
     C*
     C                   READ      PCA01                                  50
     C                   END                                                    END *IN50 DOWEQ '0'
     C*
     C* END THE PROGRAM
     C*
     C                   MOVE      '1'           *INLR
     OPHC01     E            PHC01E
