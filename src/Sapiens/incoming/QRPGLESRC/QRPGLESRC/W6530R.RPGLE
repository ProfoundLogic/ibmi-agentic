    Hcopyright('(C) Copyright Fiserv, Inc., 2004 - 2008')
    H*********************************************************************
    H/TITLE W6530R - Return Fund Check # Update Program
    H*********************************************************************
    H* CALLED FROM:                                                      *
    H*   Return Fund Batch (W6530C)                                      *
    H* CALLS TO:                                                         *
    H*   NONE                                                            *
    H*                                                                   *
    H* CREATED:  12/26/07     C:183    PROJECT: 81WZB                    *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
    H* 04/16/08 C:136 Support: WA081 62950 (81WZB)                       *
    H*                Allow just Surplus Dividend checks to be printed   *
    H*                if selected from the prompt. Corrected program     *
    H*                selection criteria. WAR4L11 was changed to         *
    H*                exclude WAR4P records that aren't selected,        *
    H*                that don't have a check date, and that have        *
    H*                been processed and are now actual RF checks.       *
    H*                                                                   *
    H* 04/07/08 C:136 Support: WA081 62972                               *
    H*                Delete call to W6560R. The processing for updating *
    H*                A/R is moved to W6550R. Populate A/R Code if       *
    H*                appropriate                                        *
    H*********************************************************************
     FWAR4L11   UF   E           K DISK
     F                                     INFDS(DBFDS)
     FWAR4P     IF   E             DISK    RENAME(PR401:PR400) PREFIX(X_)
     FWSCKL     UF   E           K DISK
      *
     D DBFDS           DS
     D  DBRRN                397    400B 0
      *
     DPSDS           ESDS                  EXTNAME(SZQ1P)

     D QQCO#           S              3
     D QQFFND          S              3
     D QQTFND          S              3
     D QQPRDT          S              8
     D QQBTYP          S              1
     D QQRTCD          S              1
     D KKCO#           S                   LIKE(R4CO#)
     D KKPRDT          S                   LIKE(R4CKDT)
     D KKACCT          S                   LIKE(R4ACCT)
     D XXCHK#          S                   LIKE(R4CHK#)
     D KKCKTP          s             10
     D DATUSA          S               D   DATFMT(*USA)
     D SYS_DATE        S               D   DATFMT(*USA)
     D ##CO#           S              3  0
     D ##CONM          S             40
     D ##FNAM          S             40
     D ##FND           S              3  0
     D #1MODE          S              1
     D #1CO#           S              3
     D #1FND           S              3
     D #1FYR           S              3
     D #1CASE          S              7
     D #1APPL          S              2
     D #1DATA          S             50
     D #1DATE          S              8
     D #1ACCT          S             20
     D #1CALL          S              1
     D #1HDCY          S              1
     D #1CHKQ          S             10
     D #1FTYP          S             10
     D #1RTCD          S              1
     D #1EFDT          S              7
     D #1CPYQ          S             10
     D #1CKTP          S             10
     D DLY5SEC         C                   CONST('DLYJOB DLY(5)')
     D ##CMD           S             14
     D ##CMDL          S             15  5
      *** beg add ***** 04/16/08 *******************************************************
     d surDivFlg       s              1
      *** end add ***** 04/16/08 *******************************************************
      *
     C     CKKY          KLIST
     C                   KFLD                    R4CO#
     C                   KFLD                    #1ACCT
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#
     C                   PARM                    QQFFND
     C                   PARM                    QQTFND
     C                   PARM                    QQPRDT
     C                   PARM                    QQBTYP
     C                   PARM                    QQRTCD
      *
      * INITIALIZE
      *
     C     *DTAARA       DEFINE                  PREMCHK          80
      *
     C     *USA          MOVE      *DATE         SYS_DATE
     C                   MOVE      QQCO#         KKCO#
     C     *USA0         MOVE      QQPRDT        DATUSA
     C     *CYMD         MOVE      DATUSA        KKPRDT
      *
     C                   MOVE      QQFFND        @@FFND            3 0
     C                   MOVE      QQTFND        @@TFND            3 0
      *
      *** beg add ***** 04/16/08 **********************************************
     c                   eval      surDivFlg = ' '
     c                   if        qqbtyp = 'S'
     c                   eval      qqbtyp = 'I'
     c                   eval      surDivFlg = 'Y'
     c                   endif
      *** end add ***** 04/16/08 **********************************************
      **********
      *** beg chg ***** 04/16/08 *******************************************************
     C**********         READ      PR401
     C**********         DOW       NOT %EOF(WAR4L11)
     C**********         IF        R4SELC = 'Y' AND R4CHK# = 0
     C**********                   AND R4STAT = *BLANK AND
     C**********                   ((QQBTYP = *BLANKS) OR
     C**********                   (R4BTYP = QQBTYP AND R4BTYP <> *BLANKS))
     C**********                   AND ((@@FFND = *ZEROS AND @@TFND = *ZEROS)
     C**********                   OR (R4FND >= @@FFND AND R4FND <= @@TFND
     C**********                   AND @@FFND <> *ZEROS AND @@TFND <> *ZEROS))
     c                   dou       %eof(WAR4L11)
     c                   read      pr401
     c                   select
     c                   when      %eof(WAR4L11)
     c                   iter
     c                   when      surDivFlg = 'Y' and R4SDFL <> 'Y'
     c                   iter
     c                   when      qqbtyp <> *blank and R4BTYP <> qqbtyp
     c                   iter
     c                   when      @@ffnd <> *zeros or @@tfnd <> *zeros
     c                   if        R4FND < @@ffnd or R4FND > @@tfnd
     c                   iter
     c                   endif
     c                   endsl
      *** end chg ***** 04/16/08 *******************************************************
      *
     C                   MOVE      R4CO#         #1CO#
     C                   MOVE      R4FND         #1FND
     C                   MOVE      R4FYR         #1FYR
      *
     C                   SELECT
     C                   WHEN      R4BTYP = 'I'
     C                   EVAL      #1APPL = 'IC'
     C                   OTHER
     C                   EVAL      #1APPL = 'RP'
     C                   ENDSL
      *
     C                   CALL      'WA023R1'
     C                   PARM      '2'           #1MODE
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C                   PARM                    #1APPL
     C                   PARM                    #1DATA
     C                   PARM      *BLANK        #1CKTP
     C                   PARM      *BLANK        #1RTCD
      *
     C                   IF        #1RTCD <> '1'
      *
     C                   EVAL      R4CKTP = #1CKTP
      *
     C                   MOVE(P)   QQPRDT        #1DATE
     C                   EVAL      KKCKTP = #1CKTP
      *
     C                   SELECT
     C                   WHEN      R4BTYP = 'I'
     C                   EVAL      #1APPL = 'IC'
     C                   OTHER
     C                   EVAL      #1APPL = 'RP'
     C                   ENDSL
      *
     C                   CALL      'WA023R'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1APPL
     C                   PARM                    #1DATE
     C                   PARM      *BLANKS       #1ACCT
     C                   PARM      'C'           #1CALL
     C                   PARM                    #1HDCY
     C                   PARM                    #1CHKQ
     C                   PARM                    #1FTYP
     C                   PARM                    #1RTCD
     C                   PARM                    #1EFDT
     C                   PARM                    #1CPYQ
     C                   PARM      KKCKTP        #1CKTP
      *
     C                   EVAL      KKACCT = #1ACCT
     C                   IF        KKACCT <> *BLANKS AND
     C                             #1HDCY = 'Y'
      *
     C     *LOCK         IN        PREMCHK
     C                   EVAL      %SUBST(PREMCHK:9:10) = #1CHKQ
     C                   EVAL      %SUBST(PREMCHK:19:10) = #1CPYQ
     C                   EVAL      %SUBST(PREMCHK:29:10) = #1FTYP
     C                   OUT       PREMCHK
      *
     C                   Z-ADD     0             SECCNT5           4 0
      *
      * UPDATE THE BANK MASTER RECORD WITH THE NEXT CHECK NUMBER
      *
     C     CKKY          CHAIN(e)  PCK01
      *
      * Logic to wait and delay if the file or record is already locked. It will repeatedly
      * chain in 5 seconds to see if it is released but not after 30 minutes.
      *
     C                   DOW       %ERROR AND SECCNT5 <= 720
     C                             AND %STATUS = 01218
      *
     C                   CALL      'QCMDEXC'
     C                   PARM      DLY5SEC       ##CMD
     C                   PARM      14            ##CMDL
      *
     C     CKKY          CHAIN(e)  PCK01
      *
     C                   EVAL      SECCNT5 = SECCNT5 + 1
      *
     C                   ENDDO                                                  DOW NOT %ERROR
      *
     C                   IF        %FOUND(WSCKL) AND NOT %ERROR
     C                   EVAL      XXCHK# = CKLCHK
     C                   EVAL      CKLCHK = CKLCHK + 1
     C     *CYMD         MOVE      SYS_DATE      CKDT
     C                   TIME                    CKTIME
     C                   EVAL      CKPGM  = Q1PGM
     C                   EVAL      CKUSER  = Q1USER
     C                   UPDATE    PCK01
     C                   ELSE
     C                   EVAL      QQRTCD = 'N'
     C                   LEAVE
     C                   ENDIF                                                  %FOUND(WSCKL)
      *
      *** beg add ***** 04/07/08 *******************************************************
     c                   select
     c                   when      R4BTYP = 'I'
     c                   when      R4BTYP = 'D'
     c                   eval      R4ARCD = CKRDCD
     c                   when      R4BTYP = 'P' or
     c                             R4BTYP = 'A'
     c                   eval      R4ARCD = CKRPCD
     c                   endsl
      *** end add ***** 04/07/08 *******************************************************
      *
     C                   EVAL      R4ACCT = #1ACCT
      *
     C                   MOVE      QQPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         R4CKDT
      *
     C                   EVAL      R4CHK# = XXCHK#
     C     *CYMD         MOVE      SYS_DATE      R4DATE
     C                   TIME                    R4TIME
     C                   EVAL      R4PGM  = Q1PGM
     C                   EVAL      R4USER = Q1USER
      *
     C                   UPDATE    PR401
      *
     C                   CALL      'W6550C'
     C                   PARM      DBRRN         @@RRN#            9 0
      *
     C     @@RRN#        CHAIN     WAR4P
      *
     C                   IF        X_R4STAT = 'Y' AND X_R4BTYP <> 'I'
     C                             AND %FOUND(WAR4P)
      *
      *** beg del ***** 04/07/08 *******************************************************
     C**********         CALL      'W6560R'
     C**********         PARM                    QQCO#
     C**********         PARM                    QQPRDT
      *** end del ***** 04/07/08 *******************************************************
      *
     C     *LOCK         IN        PREMCHK
     C                   EVAL      %SUBST(PREMCHK:9:10) = *BLANKS
     C                   EVAL      %SUBST(PREMCHK:19:10) = *BLANKS
     C                   EVAL      %SUBST(PREMCHK:29:10) = *BLANKS
     C                   OUT       PREMCHK
      *
     C                   ENDIF                                                  KKACCT <> *BLANKS
      *
     C                   ENDIF                                                  #1RTCD <> '1'
      *
     C                   ENDIF                                                  X_R4STAT = 'Y'
      *
      *** beg del ***** 04/16/08 ********************************************************
     C***********        ENDIF                                                  R4SELC = 'Y'
     C***********        READ      PR401
      *** end del ***** 04/16/08 ********************************************************
      *
     C                   ENDDO                                                  %EOF(WAR4L11)
      *
     C                   IF        QQRTCD <> 'N'
     C                   EVAL      QQRTCD = 'Y'
     C                   ENDIF                                                  QQRTCD <> 'N'
      *
     C                   EVAL      *INLR = *ON
