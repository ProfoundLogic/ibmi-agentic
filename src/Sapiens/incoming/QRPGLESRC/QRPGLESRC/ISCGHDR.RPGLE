     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE ISCGHDR  EDIT/WRITE CUST DATA RCD FOR HD (FMT A)
     H*****************************************************************
     H*                                                               *
     H*    CALLS TO:                                                 **
     H*                                                              **
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* 60  GENERAL CHAIN INDICATOR                                   *
     H* 61  GENERAL CHAIN INDICATOR                                   *
     H*****************************************************************
     H* ERR CODES
      * 001 - NOT NUMERIC VALUE
     H*
     H*****************************************************************
     H** FILES:                                                      **
     H*****************************************************************
     F*
      * Work station ID file
     FSMHDL     UF A E           K DISK
     F*

     D UA              S             10    DIM(10)
     D UD              S              7    DIM(5)
     D UN              S              9    DIM(5)
     D ER              S              3    DIM(20)
     D*
     D*
     D                 DS
     D  QQUA                   1    100
     D  QUA01                  1     10
     D  QUA02                 11     20
     D  QUA03                 21     30
     D  QUA04                 31     40
     D  QUA05                 41     50
     D  QUA06                 51     60
     D  QUA07                 61     70
     D  QUA08                 71     80
     D  QUA09                 81     90
     D  QUA10                 91    100
     D*
     D                 DS
     D  QQUD                   1     35
     D  QUD01                  1      7
     D  QUD02                  8     14
     D  QUD03                 15     21
     D  QUD04                 22     28
     D  QUD05                 29     35
     D                 DS
     D  QQUN                   1     45
     D  QUN01                  1      9
     D  QUN02                 10     18
     D  QUN03                 19     27
     D  QUN04                 28     36
     D  QUN05                 35     45
     D                 DS
     D  HDUA                   1    100
     D  HUA                    1    100
     D                                     DIM(10)
     D  HDUA01                 1     10
     D  HDUA02                11     20
     D  HDUA03                21     30
     D  HDUA04                31     40
     D  HDUA05                41     50
     D  HDUA06                51     60
     D  HDUA07                61     70
     D  HDUA08                71     80
     D  HDUA09                81     90
     D  HDUA10                91    100
     D*
     D                 DS
     D  HDUD                   1     35
     D  HUD                    1     35  0
     D                                     DIM(5)
     D  HDUD01                 1      7  0
     D  HDUD02                 8     14  0
     D  HDUD03                15     21  0
     D  HDUD04                22     28  0
     D  HDUD05                29     35  0
     D                 DS
     D  HDUN                   1     45
     D  HUN                    1     45  2
     D                                     DIM(5)
     D  HDUN01                 1      9  2
     D  HDUN02                10     18  2
     D  HDUN03                19     27  2
     D  HDUN04                28     36  2
     D  HDUN05                37     45  2
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*** DARTS CUST DATA ENTRY FILE FMT A (HD)
     C*
     C     HDKY          KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    QQCSID
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQCSID            2
     C                   PARM                    QIUA            100            10 10A
     C                   PARM                    QIUD             35            5  7/0 DTS
     C                   PARM                    QIUN             45            5  9/2 NUM
     C                   PARM                    QQERR            60            20 3A FLDS
     C                   PARM                    QQRTCD            1
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C*
     C*
     C*
      *================================================================
     C*
      * THE UA INPUT PARM IS MADE UP OF (10) 10A FIELDS
      * THE UD            IS            (5)  7/0 DATE FIELDS
      * THE UN            IS            (5)  9/2 NUMERIC FIELDS
      *
     ******************************************************************
      *
      * ** BREAK UP THE INPUT PARMS AND VALIDATE THE CONTENTS MEET
      * ** THE DATA TYPE REQUIREMENT (FOR NUMERICS)
      * ** IF NOT, BYPASS THE BAD DATA ELEMENT.
      *
      * ** GET THE CUSTGROUPID FILE TO GET THE VALIDATION INFORMATION
      * ** VALIDATE EACH FIELD THAT SHOULD BE VALIDATED.
      * ** IF ERRORS OCCUR, KEEP TRACK OF THEM IN AN ARRAY, AND AT
      * ** END(RETRN) TIME, WE MUST RETURN THE ERRORS FOR EACH FIELD.
      * ** WILL USE A SINGLE ALPHA FIELD MADE UP OF 3A FOR EACH ERROR.
      *
      *
      *
     ******************************************************************
     C*** CLEAR ALL DATA FIELDS
     C*
     C                   MOVE      *BLANKS       ER
     C*
     C                   MOVE      *BLANKS       UA
     C                   MOVE      *ZEROS        UD
     C                   MOVE      *ZEROS        UN
     C*
     C                   MOVE      *BLANKS       HUA
     C                   MOVE      *ZEROS        HUD
     C                   MOVE      *ZEROS        HUN
     C*
     C                   MOVE      *BLANKS       QQUA
     C                   MOVE      *ZEROS        QQUD
     C                   MOVE      *ZEROS        QQUN
     C                   MOVE      *BLANKS       HDUA
     C                   MOVE      *ZEROS        HDUD
     C                   MOVE      *ZEROS        HDUN
     C*
     C                   MOVEL     QIUA          QQUA
     C                   MOVEL     QIUD          QQUD
     C                   MOVEL     QIUN          QQUN
     C*
     C                   EXSR      VALDTA
     C*
     C                   EXSR      EDTDTA
     C*
     C                   MOVEA     ER            QQERR
     C*
     C     QQERR         IFNE      *BLANKS
     C                   MOVE      '1'           QQRTCD
     C                   GOTO      TERM
     C                   ELSE
     C                   MOVE      *BLANKS       QQRTCD
     C                   ENDIF
     C*
     C                   EXSR      WRTDTA
     C*
     C*
     C     TERM          TAG
     C                   RETURN
     C     EXIT          TAG
     C                   MOVE      '1'           *INLR
     C*
     C     VALDTA        BEGSR
     C*
     C                   MOVEA     QQUA          UA
     C*
     C*
     C                   MOVEA     QQUD          UD
     C                   DO        5             X                 3 0
     C     X             ADD       10            Y                 3 0
     C*
     C     UD(X)         IFEQ      *BLANKS
     C     UD(X)         OREQ      *ALL'0'
     C*** THATS OK....
     C                   MOVE      *ZEROS        UD(X)
     C                   GOTO      BPS1
     C                   ENDIF
     C                   TESTN                   UD(X)                66
     C     *IN66         IFEQ      *OFF
     C                   MOVE      '001'         ER(Y)
     C                   ENDIF
     C*
     C     BPS1          TAG
     C                   ENDDO
     C*
     C*
     C*
     C                   MOVEA     QQUN          UN
     C                   DO        5             X                 3 0
     C     X             ADD       15            Y                 3 0
     C*
     C     UN(X)         IFEQ      *BLANKS
     C     UN(X)         OREQ      *ALL'0'
     C*** THATS OK....
     C                   MOVE      *ZEROS        UN(X)
     C                   GOTO      BPS2
     C                   ENDIF
     C                   TESTN                   UN(X)                66
     C     *IN66         IFEQ      *OFF
     C                   MOVE      '001'         ER(Y)
     C                   ENDIF
     C*
     C     BPS2          TAG
     C                   ENDDO
     C*
     C     VALEND        ENDSR
     C*
     C     EDTDTA        BEGSR
     C*
     C*** GET THE CG RCD TO DETERMINE WHAT EACH FIELDS EDITS ARE.
     C*
     C*** FOR EACH FIELD, IF IT DOESNT ALREADY HAVE AN ERROR,
     C*** PERFORM THE SPECIFIC VALIDATAIONS
     C***
     C*
     C*
     C*** USING THE CG DATA, ONLY PERFORM EDITS ON FIELDS THAT
     C*
     C*** 1) ARE SUPPOSED TO HAVE DATA
     C*** 2) HAVE ONE OF THE CG EDITS DEFINED (MANDATORY, DATE,ETC)
     C*
     C*
     C*
     C     EDTEND        ENDSR
     C*
     C     WRTDTA        BEGSR
     C*
     C*** CHAIN TO THE HD FILE,
     C*** IF EXISTS UPDATE THE APPROPRIATE FIELDS (BASED ON THE CG INF)
     C***
     C*** ELSE WRITE THE DATA BASED ON THE CG INF
     C*
     C     HDKY          CHAIN     PHD01                              60
     C                   MOVEA     UA            HUA
     C*
     C                   DO        5             X
     C                   MOVE      UD(X)         HUD(X)
     C                   MOVE      UN(X)         HUN(X)
     C                   ENDDO
     C*
     C                   MOVE      *BLANKS       QQRTCD
     C     *IN60         IFEQ      *ON
     C                   MOVEL     QQKEY         HDKEY
     C                   MOVEL     QQCSID        HDCSID
     C*** MUST FILL IN THE PNAM
     C*                    MOVELCGXXXX    HDPNAM
     C                   WRITE     PHD01
     C                   ELSE
     C                   UPDATE    PHD01
     C                   ENDIF
     C*
     C*
     C*
     C     WRTEND        ENDSR
