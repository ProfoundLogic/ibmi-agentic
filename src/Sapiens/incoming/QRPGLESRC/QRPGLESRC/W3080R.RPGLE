     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W3080R - DELETE RECORDS FROM REQUEST FILE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  02/07/13 C:136 Project: SY336 Priss #68                      *
     H*                 Use new EFT flag to trigger writing EFT       *
     H*                 instead of R2CKTP                             *
     H*                                                               *
     H*  02/28/13 C:283 Support: WA071 81681                          *
     H*                 Moving the functionality that creates the     *
     H*                 WAEFP records to the place where WAR2P records*
     H*                 are deleted from program W3030R1.             *
     H*                                                               *
     H*  09/13/01 C:132  Project:  WC924                              *
     H*           Add CICenter Commission Extract File as output      *
     H*           ADD a record to WHXAP for each Group Processed      *
     H*                                                               *
     H*  12/02/97 C:30   Project:  WC500                              *
     H*           Change DATE PARM from 6 to 8 chars.                 *
     H*                                                               *
     H*****************************************************************
     FWAR2L1    UF   E           K DISK
     F**** BEGIN ADD **** 02/28/13 *********************************************
     F                                     infds(dbfds)
     FWMEFL     IF   E           K DISK
     F                                     infds(dbfds1)
     FWMAAL1    IF   E           K DISK
     F**** END   ADD **** 02/28/13 *********************************************
     FWAC2L     IF   E           K DISK
     F**** BEGIN ADD **** 09/13/01 *********************************************
     FWHXAP     UF A E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     F**** END ADD ****** 09/13/01 *********************************************

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D**** BEGIN CHG **** 09/13/01 *********************************************
     D* Change Date/time Field to 14, Date to 8
     D**********                              1  120DSDTTM
     D**********                              1   60DSTIME
     D**********                              7  120DSDATE
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     I**** END CHG ****** 09/13/01 *********************************************
     D**** BEGIN ADD **** 02/28/13 *********************************************
     D dbfds           ds
     D  dbrrn                397    400b 0
     D dbfds1          ds
     D  dbrrn1               397    400b 0
     D**** END   ADD **** 02/28/13 *********************************************
      *** beg add ***** 02/07/14 ***********************************************
     d eftType         s                   like(EFtype) inz('A')
      *** end add ***** 02/07/14 ***********************************************

     C*
     C     *LIKE         DEFINE    C2DT          XXDT
     C*
     C     *ENTRY        PLIST
      *** BEGIN CHG *** 12/02/97 ***********************************************
     C*********************PARM***********QQDATE  6
     C                   PARM                    QQDATE            8
      *** END   CHG *** 12/02/97 ***********************************************
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
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
     C*
      *** BEGIN CHG *** 12/02/97 ***********************************************
     C*********************MOVE QQDATE    XXDATE  60
      *
     C                   MOVE      QQDATE        XXDATE            8 0
      *** END   CHG *** 12/02/97 ***********************************************
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C*
     C*  REQUEST PAYMENT KEY LIST
     C*
     C     R2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  COMMISSION CHECK PROCESSING KEY LIST
     C*
     C     C2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C                   KFLD                    XXFND
     C*
     C**** BEGIN ADD **** 09/13/01 *********************************************
     C*
     C*  CICenter Extract File Key List
     C*
     C     XAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXDT
     C**** END ADD ****** 09/13/01 *********************************************
     C**** BEGIN ADD **** 02/28/13 *********************************************
     C*
     C*  Agent Assignment Master File key list
     C*
     C     AAKY1         KLIST
     C                   KFLD                    R2CO#
     C                   KFLD                    R2FND
     C                   KFLD                    R2AGT#
     C*
     C*  WMEFL key
     C*
     C     EFKY          KLIST
     C                   KFLD                    R2CO#
      *** beg chg ***** 02/07/14 ******************************
     C*****************  KFLD                    R2COTP
     C                   KFLD                    eftType
      *** beg chg ***** 02/07/14 ******************************
     C                   KFLD                    R2FND
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*  WMEFL Key
     C*
     C     EFKY1         KLIST
     C                   KFLD                    R2CO#
      *** beg chg ***** 02/07/14 ******************************
     C*****************  KFLD                    R2COTP
     C                   KFLD                    eftType
      *** beg chg ***** 02/07/14 ******************************
     C                   KFLD                    R2FND
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C                   KFLD                    XXDT
     C**** END ADD ****** 02/28/13 *********************************************
     C*
     C*  START READING THE CHECK PROCESSING FILE, WE MUST HAVE
     C*  A RECORD FOR THIS DATE
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C     C2KEY         CHAIN     PC201                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  THE CHECK PROCESSING FLAG MUST BE FLIPPED
     C*
     C     C2STA0        IFEQ      'Y'
     C     C2STA1        ANDEQ     'Y'
     C*
     C*  START READING ALL PAYMENT RECORDS
     C*
     C     R2KEY         CHAIN     PR201                              60
     C     *IN60         DOWEQ     '0'
     C**** BEGIN ADD **** 02/28/13 *********************************************
     C*** beg chg ***** 02/07/14 ***********************************************
     C*****R2CKTP        IFEQ      'EFT'
     C     R2EFTC        IFEQ      'E'
     C*** end chg ***** 02/07/14 ***********************************************
      *
     C     AAKY1         CHAIN     PAA01                              60
     C     *IN60         IFEQ      *OFF
      *
     C     EFKY1         SETGT     PEF01
     C     EFKY          READPE    PEF01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      * FIND EFFECTIVE RECORD, I.E. BATCH DATE IN BETWEEN EFFECTIVE AND EXPIRY DATES
     C     XXDT          IFGE      EFEFDT
     C     XXDT          ANDLT     EFEXDT
     C     EFEXDT        OREQ      0
      *
      * Call API to write to EFT central holding file API
     C                   CALL      'WA073R'
     C                   PARM      'A'           ##TYPE            1
     C                   PARM      DBRRN         ##RRN             9 0
     C                   PARM      DBRRN1        ##RRN1            9 0
     C                   PARM      ' '           ##RTN             1
     C*
     C                   ENDIF
     C     EFKY          READPE    PEF01                                  60
      *
     C                   ENDDO
      *
     C                   ENDIF
     C                   ENDIF
     C**** END   ADD **** 02/28/13 *********************************************
     C*
     C*  DELETE PROCESSED OR DELETED RECORD FROM WAR2P
     C*
     C     R2STAT        IFEQ      'Y'
     C     R2STAT        OREQ      'D'
     C                   DELETE    PR201
     C                   END
     C*
     C     R2KEY         READE     PR201                                  60
     C                   END
     C*
     C**** BEGIN ADD **** 09/13/01 *********************************************
     C                   CLEAR                   WCFL19
     C     'WC'          CHAIN     SWC01                              60
     C  N60WCFL19        COMP      'Y'                                    96
     C     *IN96         CABEQ     *OFF          NOCIC
     C*  Check for Existing WHXAP record and Add if not Found. Only change
     C*  the Maintenance Fields if Found
     C*
     C                   CLEAR                   PXA01
     C*
     C     *LIKE         DEFINE    C2DT          XXADT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXADT
     C*
     C     XAKY          CHAIN     PXA01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     XXCO#         XACO#
     C                   Z-ADD     XXFND         XAFND
     C                   Z-ADD     XXDT          XAPRDT
     C                   MOVEL     Q1USER        XAUSER
     C                   MOVEL     XXADT         XADT
     C                   MOVE      DSTIME        XATIME
     C                   MOVEL     Q1PGM         XAPGM
     C                   MOVEL     Q1USER        XAIUSR
     C                   MOVEL     XXADT         XAIDT
     C                   MOVE      DSTIME        XAITIM
     C                   MOVEL     Q1PGM         XAIPGM
     C                   WRITE     PXA01
     C*
     C                   ELSE
     C*
     C                   MOVEL     Q1USER        XAUSER
     C                   MOVEL     XXADT         XADT
     C                   MOVE      DSTIME        XATIME
     C                   MOVEL     Q1PGM         XAPGM
     C                   UPDATE    PXA01
     C                   ENDIF                                                  END 60 EQ ON
     C* No CICenter Flag TAG
     C     NOCIC         TAG
     C*
     C**** END ADD ****** 09/13/01 *********************************************
     C*
     C                   END
     C*
     C                   END
     C*
     C                   MOVE      '1'           *INLR
