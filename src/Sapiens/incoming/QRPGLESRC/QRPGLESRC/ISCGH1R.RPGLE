     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE ISCGH1R  EDIT/WRITE CUST DATA RCD FOR H1 (FMT B)
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
     FSMH1L     UF A E           K DISK
     F*

     D UB              S             35    DIM(6)
     D ER              S              3    DIM(20)
     D*
     D*
     D                 DS
     D  QQUB                   1    210
     D  QUB01                  1     35
     D  QUB02                 36     70
     D  QUB03                 71    105
     D  QUB04                106    140
     D  QUB05                141    175
     D  QUB06                176    210
     D*
     D*
     D                 DS
     D  H1UB                   1    210
     D  HUB                    1    210
     D                                     DIM(6)
     D  H1UB01                 1     35
     D  H1UB02                36     70
     D  H1UB03                71    105
     D  H1UB04               106    140
     D  H1UB05               141    175
     D  H1UB06               176    210
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*** DARTS CUST DATA ENTRY FILE FMT A (H1)
     C*
     C     H1KY          KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    QQCSID
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQCSID            2
     C                   PARM                    QIUB            210             6 35A
     C                   PARM                    QQERR            18             6 3A FLDS
     C                   PARM                    QQRTCD            1
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C*
     C*
     C*
      *================================================================
     C*
      * THE UB INPUT PARM IS MADE UP OF (6) 35 FIELDS
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
     C                   MOVE      *BLANKS       UB
     C*
     C                   MOVE      *BLANKS       HUB
     C*
     C                   MOVE      *BLANKS       QQUB
     C                   MOVE      *BLANKS       H1UB
     C*
     C                   MOVEL     QIUB          QQUB
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
     C                   MOVEA     QQUB          UB
     C*
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
     C*** CHAIN TO THE H1 FILE,
     C*** IF EXISTS UPDATE THE APPROPRIATE FIELDS (BASED ON THE CG INF)
     C***
     C*** ELSE WRITE THE DATA BASED ON THE CG INF
     C*
     C     H1KY          CHAIN     PH101                              60
     C                   MOVEA     UB            HUB
     C*
     C*
     C                   MOVE      *BLANKS       QQRTCD
     C     *IN60         IFEQ      *ON
     C                   MOVEL     QQKEY         H1KEY
     C                   MOVEL     QQCSID        H1CSID
     C*** MUST FILL IN THE PNAM
     C*!!!!!!!!!!!!!!!!!!  MOVELCGXXXX    H1PNAM
     C                   WRITE     PH101
     C                   ELSE
     C                   UPDATE    PH101
     C                   ENDIF
     C*
     C*
     C*
     C     WRTEND        ENDSR
