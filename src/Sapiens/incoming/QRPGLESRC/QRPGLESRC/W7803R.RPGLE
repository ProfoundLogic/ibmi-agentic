     H*  (C) Copyright Progressive Data Solutions, Inc., 2005
     H/TITLE W7803R - PROMPT FOR SUBMIT OF 3RD PART CERT UPLOAD PROCESSOR
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 01/24/08 C:136 WA135 61724                                    *
     H*                Edit zip code field for numeric values         *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
      * PEO flat file
     FWZCEP     UF   E             DISK
     F                                     INFDS(infWZCEP)
     FWMCEP     IF A E             DISK
     F                                     RENAME(PCE01:DBRCDF)
     F                                     PREFIX(X)
     FWMCEL6    IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
      *
     D ERROR           S                   LIKE(*IN99)
     D A               S              3  0 INZ
     D B               S              3  0 INZ
     D ERRMSG          S             30
      * Constants
     D MSG1            C                   CONST('Alpha value in numer-
     D                                     ic field')
     D MSG2            C                   CONST('Already processed')
     D MSG3            C                   CONST('Processed Successful-
     D                                     ly')
     D MSG4            C                   CONST('CO#/GRP not found')
     D MSG5            C                   CONST('CO#/GRP/EMP/DIV not -
     D                                     found')
     D MSG6            C                   CONST('CO#/GRP/EMP/DIV/YEAR-
     D                                      not found')

      *
     D infWZCEP        DS
     D  $FILE                  1      8
     D  $STATUS          *STATUS
     D  $ROUTINE         *ROUTINE
     D  $LINENUM              30     37
     D  $FILLIBR              93    102
     D  $FILMBER             129    138
     D  $RRN                 397    400B 0
      *-----------------------------------------------
      *  P R O G R A M   D A T A   S T R U C T U R E
      *-----------------------------------------------
      *
     D @PSDS          SDS           429
     D  @JOBDATE             191    198                                         job date MDCY
     D  @JOBCENTRY           199    200S 0                                      job century
     D  @WRKSTN              244    253                                         job name
     D  @USERID              254    263                                         user name
     D  @JOBNUMBER           264    269S 0                                      job number
     D  @JOBUDATE            270    275S 0                                      job date MDY
     D  @PGMNAME             334    343                                         program name
     D  @MODUNAME            344    353                                         module name
      *
     D  Dsdttm                 1     12  0
     D  Dstime                 1      6  0
     D  Dsdate                 7     12  0
     D SYS_DATE        S               D   DATFMT(*USA)
     D SYS_TIME        S               T   TIMFMT(*HMS:)
     D NUMB            S              1    DIM(15)
     D WKNUMB          S              1    DIM(15)
      *
      *
     C     CEKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XCEFND
     C                   KFLD                    XCEFYR
     C                   KFLD                    XCEEMP#
     C                   KFLD                    XCEDIV
     C                   KFLD                    XCECER#
     C     CEKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XCEFND
     C                   KFLD                    XCEFYR
     C                   KFLD                    XCEEMP#
     C                   KFLD                    XCEDIV
     C     FNKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XCEFND
     C     EMKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XCEFND
     C                   KFLD                    XCEEMP#
     C                   KFLD                    XCEDIV
     C     ELKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XCEFND
     C                   KFLD                    XCEEMP#
     C                   KFLD                    XCEDIV
     C                   KFLD                    XCEFYR
      *
      *-----------------------------------------------
      *  MAINLINE
      *-----------------------------------------------
      *
     C                   READ      CZC01                                  LR
     C     *INLR         DOWEQ     *OFF
     C                   EVAL      RPCO#     = *BLANK
     C                   EVAL      RPCER#    = *BLANK
     C                   MOVEL     CZPEO         RPPEO            20
     C                   MOVEL     CZNAME        RPNAME           34
     C                   IF        CZPROC   <> 'Y'
     C                   EVAL      ERROR     = *OFF
     C                   EXSR      EDIT
     C                   EVAL      CZIUSER  = @USERID
     C                   EVAL      CZIPGM   = @PGMNAME
     C                   TIME                    TIME              6 0
     C                   MOVE      TIME          CZITIME
     C     *USA          MOVE      *DATE         SYS_DATE
     C     *CYMD         MOVE      SYS_DATE      CZDT
     C                   IF        ERROR
     C                   EVAL      CZPROC   = 'E'
     c                   if        *inof     = *on
     c                   except    header
     c                   endif
     C                   EXCEPT    DETAIL
     C                   ELSE
     C                   EVAL      CZPROC   = 'Y'
     C                   EXSR      WRITERCD
     C                   ENDIF
     C     *USA          MOVE      *DATE         SYS_DATE
     C     *CYMD         MOVE      SYS_DATE      CZIDT
     C                   TIME                    TIME
     C                   MOVE      TIME          CZITIME
     C                   UPDATE    CZC01
     C                   ELSE
     C                   EVAL      ERRMSG   =  MSG2
     c                   if        *inof     = *on
     c                   except    header
     c                   endif
     C                   EXCEPT    DETAIL
     C                   ENDIF                                                  CZPROC<>'P'
     C                   READ      CZC01                                  LR
     C                   ENDDO
      *
      *-----------------------------------------------
      *  EDIT RECORD
      *-----------------------------------------------
      *
     C     EDIT          BEGSR

     C                   DO
     C                   TESTN                   CZFND                99
     C                   IF        *IN99    = *OFF
     C                   MOVE      CZFND         WKNUM            15
     C                   EXSR      FORMAT
     C                   IF        NOT ERROR
     C                   MOVE      WKNUM         XCEFND
     C                   ELSE
     C                   EVAL      ERRMSG   =  MSG1
     C                   LEAVE
     C                   ENDIF
     C                   ELSE
     C                   MOVE      CZFND         XCEFND
     C                   ENDIF

     C                   TESTN                   CZFYR                99
     C                   IF        *IN99    = *OFF
     C                   MOVE      CZFYR         WKNUM
     C                   EXSR      FORMAT
     C                   IF        NOT ERROR
     C                   MOVE      WKNUM         XCEFYR
     C                   ELSE
     C                   EVAL      ERRMSG   =  MSG1
     C                   LEAVE
     C                   ENDIF
     C                   ELSE
     C                   MOVE      CZFYR         XCEFYR
     C                   ENDIF

     C                   TESTN                   CZDATE               99
     C                   IF        *IN99    = *OFF
     C                   MOVE      CZDATE        WKNUM
     C                   EXSR      FORMAT
     C                   IF        NOT ERROR
     C                   MOVE      WKNUM         CEEFFDAT          8 0
     C                   ELSE
     C                   EVAL      ERRMSG   =  MSG1
     C                   LEAVE
     C                   ENDIF
     C                   ELSE
     C                   MOVE      CZDATE        CEEFFDAT
     C                   ENDIF

     C                   TESTN                   CZEMP#               99
     C                   IF        *IN99    = *OFF
     C                   MOVE      CZEMP#        WKNUM
     C                   EXSR      FORMAT
     C                   IF        NOT ERROR
     C                   MOVE      WKNUM         XCEEMP#
     C                   ELSE
     C                   EVAL      ERRMSG   =  MSG1
     C                   LEAVE
     C                   ENDIF
     C                   ELSE
     C                   MOVE      CZEMP#        XCEEMP#
     C                   ENDIF

     C                   TESTN                   CZDIV                99
     C                   IF        *IN99    = *OFF
     C                   MOVE      CZDIV         WKNUM
     C                   EXSR      FORMAT
     C                   IF        NOT ERROR
     C                   MOVE      WKNUM         XCEDIV
     C                   ELSE
     C                   EVAL      ERRMSG   =  MSG1
     C                   LEAVE
     C                   ENDIF
     C                   ELSE
     C                   MOVE      CZDIV         XCEDIV
     C                   ENDIF
      *
      *** BEG ADD ***** 01/24/08 *******************************************************
     C                   TESTN                   CZZIP                99
     C                   IF        *IN99    = *OFF
     C                   MOVE      CZZIP         WKNUM
     C                   EXSR      FORMAT
     C                   IF        NOT ERROR
     C                   MOVE      WKNUM         XCEZIP
     C                   ELSE
     C                   EVAL      ERRMSG   =  MSG1
     C                   LEAVE
     C                   ENDIF
     C                   ELSE
     C                   MOVE      CZZIP         XCEZIP
     C                   ENDIF
      *** BEG ADD ***** 01/24/08 *******************************************************
      *
      * TEST GROUP
      *
     C     FNKY1         CHAIN     WMFNL                              99
     C                   IF        *IN99  =  *ON
     C                   EVAL      ERRMSG = MSG4
     C                   EVAL      ERROR   =  *ON
     C                   LEAVE
     C                   ENDIF
      *
      * TEST EMPLOYER
      *
     C     EMKY1         CHAIN     WMEML                              99
     C                   IF        *IN99  =  *ON
     C                   EVAL      ERRMSG = MSG5
     C                   EVAL      ERROR   =  *ON
     C                   LEAVE
     C                   ENDIF
      *
      *
      * TEST FUND YEAR
      *
     C     ELKY1         CHAIN     WDELL                              99
     C                   IF        *IN99  =  *ON
     C                   EVAL      ERRMSG = MSG6
     C                   EVAL      ERROR   =  *ON
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDDO

     C                   ENDSR
      *
      *-----------------------------------------------
      *  FORMAT FIELDS THAT SHOULD CONTAIN NUMERIC
      *-----------------------------------------------
      *
     C     FORMAT        BEGSR
     C                   MOVEA     WKNUM         WKNUMB
     C                   MOVEA     *ALL'0'       NUMB
     C                   EVAL      A         =  15
     C                   EVAL      B         =  15
     C                   DO        15
     C                   IF        WKNUMB(A) <>  *BLANK
     C                   MOVE      WKNUMB(A)     NUMB(B)
     C                   EVAL      B   =  B - 1
     C                   ENDIF
     C                   EVAL      A   =  A - 1
     C                   ENDDO
     C                   MOVEA     NUMB          WKNUM
     C                   TESTN                   WKNUM                99
     C                   IF        *IN99   =  *OFF
     C                   EVAL      ERROR   =  *ON
     C                   ENDIF
     C                   ENDSR
      *
      *-----------------------------------------------
      *  WRITE RECORDS TO CERTIFCATE MASTER
      *-----------------------------------------------
      *
     C     WRITERCD      BEGSR
     C
      *
     C                   EVAL      XCECO#  =  UPCO#
     C                   EVAL      XCENAME =  CZNAME
     C                   EVAL      XCEADR1 =  CZMAD1
     C                   EVAL      XCEADR2 =  CZMAD2
     C                   EVAL      XCECTY  =  CZCTY
     C                   EVAL      XCEST   =  CZST
      *** beg del ***** 01/24/08 *******************************************************
     C*****************  MOVEL     CZZIP         XCEZIP
      *** end del ***** 01/24/08 *******************************************************
     C                   EVAL      XCEATTN =  CSATTN
     C                   EVAL      XCERLFL =  'Y'
     C                   EVAL      XCEREM1 =  CZOPDS
     C                   EVAL      XCEREM2 =  *BLANK
     C                   EVAL      XCEREM3 =  *BLANK
     C                   EVAL      XCEREM4 =  *BLANK
     C                   EVAL      XCEREM5 =  *BLANK
     C                   EVAL      XCESTAT =  'A'
     C                   TIME                    TIME              6 0
     C                   EVAL      XCEUSER =  @USERID
     C                   EVAL      XCEPGM  =  @PGMNAME
     C                   TIME                    TIME
     C                   MOVE      TIME          XCETIME
     C     *USA          MOVE      *DATE         SYS_DATE
     C     *CYMD         MOVE      SYS_DATE      XCEDT
     C                   EVAL      CEIUSR  =  @USERID
     C                   EVAL      CEIPGM  =  @PGMNAME
     C                   MOVE      TIME          XCEITM
     C     *USA          MOVE      *DATE         SYS_DATE
     C     *CYMD         MOVE      SYS_DATE      XCEIDT
     C                   EVAL      XCEBUL1  =  1
     C                   EVAL      XCEWVFL   = *blanks
     C                   EVAL      XCECTG1   = *blanks
     C                   EVAL      XCETYPE   = *blanks
     C                   EVAL      XCE3PTY   = 'Y'
     C                   EVAL      XCEEOFL   = *BLANKS
      *
      *  DEFAULT LATEST # IN
      *
     C                   EVAL      XCECER#  =  *HIVAL
     C     CEKY1         SETGT     PCE01
     C     CEKY2         READPE(N) PCE01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C                   EVAL      XCECER#  =  CECER#
     C                   ELSE
     C                   EVAL      XCECER#  =  *ZERO
     C                   END
     C                   EVAL      XCECER# =  XCECER# + 1
     C                   MOVE      XCECO#        RPCO#             3
     C                   MOVE      XCECER#       RPCER#            5

     C                   WRITE     DBRCDF
     C                   EVAL      ERRMSG   =  MSG3
     c                   if        *inof     = *on
     c                   except    header
     c                   endif
     C                   EXCEPT    DETAIL
     C*
     C                   ENDSR
      *
     C     *INZSR        BEGSR
     C*
     c                   time                    dsdttm
     C     @USERID       CHAIN     PUP01                              60
     C                   EXCEPT    HEADER
     C                   Move      *Off          *Inof
     C*

     C                   ENDSR

     OQSYSPRT   E            HEADER         1 06
     O                       Dstime                 '  :  :  '
     O                                           30 'REPORT TITLE:'
     O                                        +   1 '3rd Party Certificat'
     O                                        +   0 'e Upload'
     O                       Dsdate        Y    132
     O          E            HEADER         1
     O                                          122 'PAGE:'
     O                       Page          Z    132
     O          E            HEADER         1
     O                                           10 'PEO Name'
     O                                           26 'Co#'
     O                                           43 'FND FYR Eff Date'
     O                                           65 'Cert#'
     O                                           84 'Certificate Holder'
     O                                           52 'Employer'
     O                                           57 'Div'
     O                                          118 'Error Description'
     O          E            DETAIL         1
     O                       RPPEO               22
     O                       RPCO#               26
     O                       CZFND               30
     O                       CZFYR               34
     O                       CZDATE              43
     O                       CZEMP#              53
     O                       CZDIV               59
     O                       RPCER#              65
     O                       RPNAME             100
     O                       ERRMSG             131
