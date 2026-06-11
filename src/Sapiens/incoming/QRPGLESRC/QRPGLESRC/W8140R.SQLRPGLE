    HCOPYRIGHT('(C) Copyright Fiserv, Inc., 2006')
    H*********************************************************************
    H/TITLE A/R Reconciliation Work File Build-Prompt
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DEBUG(*YES)
      /Define WEB
    H*********************************************************************
    H*  CALLED FROM: MENU                                                *
    H*                                                                   *
    H*  CALLS TO:  W8140C                                                *
    H*                                                                   *
    H*  CREATED: 11/20/06      C:183     PROJECT: 132WHL                 *
    H*********************************************************************
    H*  REVISIONS                                                        *
     H*                                                               *
     H* 05/26/08 C:183 Support: WA132/63557                           *
     H*                Changed the logic to query PDSTST/PDSFIL       *
     H*                WFA2P rather than PDSRPT WFA2P.                *
     H*                                                               *
     H* 04/27/07 C:183 Support: WA081/58156                           *
     H*                Added the format of date submission in the     *
     H*                display and made some alignment changes.       *
     H*                                                               *
     H* 03/19/07 C:183 PROJECT: 132WIA - Change in A2CYM format from  *
     H*                CYYMM to YYYYMM implemented.                   *
     H*                                                               *
     H* 01/24/07 C:183 PROJECT: 132WHL - Activate F7 to Submit for    *
     H*                Client 132.                                    *
    H*                                                                   *
    H*********************************************************************
     H*
     *********************************************************************
    *  File Specification                                               *
     *********************************************************************
      *
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     FW8140F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
      *
     *********************************************************************
    *  Definition Specification                                         *
     *********************************************************************
      *
     D/COPY SCOPYPR01D
     *---------------------------------------------------------------------
     * Stand Alone Fields
     *---------------------------------------------------------------------
     D OPTNM           C                   CONST('A/R Reconciliation Work File +
     D                                            Build')
     D XXCALL          S                   LIKE(@@CALL)
     D ##CO#           S              3  0
     D SCPRDS          S             40
     D @COUNT          S             10  0 INZ
     D @MONENDDT       S              5  0
     D @SUFFIX         S              3    INZ
     D @LNAME          S             10    INZ
     D @CMD            S            100
     D @LEN            S             15P 5
     D @SCRPDT         S              8    INZ
      *** BEGIN ADD *** 03/19/07 **************************************
     D SRDTI           S              8  0
     D SRDTO           S             10
     D SRERR           S              1
     D SRFFMT          S              8
     D SRTFMT          S              8
      *** END   ADD *** 03/19/07 **************************************

     *---------------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *---------------------------------------------------------------------
     D*ENTRY
     D W8140R          PR
     D  @@PRDS                       40
     D  @@CALL                        1
     D W8140R          PI
     D  @@PRDS                       40
     D  @@CALL                        1

     D SUBMITTER       PR                  EXTPGM('W8140C')
     D  @@CO#                         3    CONST
     D  @@RPDT                        5    CONST
      *
     *********************************************************************
    *  Copybooks for Prototypes                                         *
     *********************************************************************
      *
     D/COPY SPRSTRING
     D/COPY SPRDATE
     D/COPY SPRCOMPANY
     D/COPY WPRUSRPRMS
      *
     *********************************************************************
    *  Copybooks for Main Line of Code                                  *
     *********************************************************************
      *
      *** BEGIN ADD *** 03/19/07 **************************************
     C     DATCHK        PLIST
     C                   PARM                    SRDTI
     C                   PARM                    SRFFMT
     C                   PARM                    SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *** END   ADD *** 03/19/07 **************************************
     C/COPY SCOPYPR01C
      *
    C*====================================================================
    * DSCAPI - Call API for getting descriptions (use 91 on error)
    C*====================================================================
     C     DSCAPI        BEGSR
    C*
     C                   CLEAR                   ##CO#
     C                   EVAL      ##CO#  = USERCO#(Q1USER)
     C                   EVAL      ##CONM = CENTERFLD(GETCOMPNAM(##CO#):
     C                                                %len(SCPRDS))
     C                   EVAL      *IN91 = ##CO# = *ZERO
     C
     C                   ENDSR
     *====================================================================
    * ERRCK1 - Error checking for srceen 1 (SCR01)
     *====================================================================
     C     ERRCK1        BEGSR

      *****  Code to check whether it is a valid Month-end Date  ******

     C                   EVAL      @SCRPDT = '01' + %EDITC(SCRPDT:'X')
     C     *EUR0         TEST(DE)                @SCRPDT
     C                   IF        %ERROR = '1'
     C                   EVAL      *IN90 = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      *IN43 = *ON
     C*                  EVAL      SCMSGFLD = 'Enter a valid Month-end Date'
     C                   ELSE
     C                   EVAL      *IN90 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN43 = *OFF
     C*                  EVAL      SCMSGFLD = *BLANKS
     C                   ENDIF

     C                   IF        *IN90 = *OFF
     C*                  EVAL      SCMSGFLD = *BLANKS

      ********  Conversion of MMYYYY format to CYMM format  ********
     C                   EVAL      @MONENDDT = DT5CYMD(SCRPDT)
      *** BEGIN ADD *** 03/19/07 **************************************
      *
     C                   MOVE      @SCRPDT       SRDTI
     C                   EVAL      SRFFMT = '*DMYY'
     C                   EVAL      SRTFMT = '*YYMD'
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         @@MONENDDT        6 0
      *
      *** END   ADD *** 03/19/07 **************************************
      ***  Code to check whether this month-end data is already  ****
      **************  present in PDSRPT/WFA2P ***********************
      *
     C                   EVAL      @COUNT = 0
      *
     C                   CALL      'SA030R'
     C                   PARM                    @SUFFIX
      *
     C*** BEG DEL ***** 05/26/08 **************************************
     C*************      EVAL      @LNAME = 'PDSRPT' + @SUFFIX
      *
     C*************      EVAL      @CMD = 'ADDLIBLE LIB(' + @LNAME + ')'
      *
     C*************      CALL      'QCMDEXC'
     C*************      PARM                    @CMD
     C*************      PARM      100           @LEN
     C*** END DEL ***** 05/26/08 **************************************
      *
     C/EXEC SQL
      *** BEGIN CHG *** 03/19/07 **************************************
     C*+ SELECT COUNT(*) INTO :@COUNT FROM WFA2PSAV WHERE A2CYM = :@MONENDDT
     C+ SELECT COUNT(*) INTO :@COUNT FROM WFA2P WHERE A2MEDT = :@@MONENDDT
      *** END   CHG *** 03/19/07 **************************************
     C/END-EXEC
      *
     C*** BEG DEL ***** 05/26/08 **************************************
     C**************     EVAL      @CMD = 'RMVLIBLE LIB(' + @LNAME + ')'
      *
     C**************     CALL      'QCMDEXC'
     C**************     PARM                    @CMD
     C**************     PARM      100           @LEN
     C*** END DEL ***** 05/26/08 **************************************
      *
     C                   IF        @COUNT > 0
      *** BEG CHG ***** 01/24/07 **************************************
      *
     C                   DOW       *INKL = '0'
      *
      *** END CHG ***** 01/24/07 **************************************
     C                   EXFMT     INFOWDW
      *** BEG CHG ***** 01/24/07 **************************************
      *
     C                   SELECT
      *
     C                   WHEN      *INKL = *ON
     C                   EVAL      *IN90 = '1'
     C                   EVAL      *INKL = '0'
     C                   LEAVE
      *
      *** SUBMIT THE EXTRACT ONLY WHEN F7 KEY IS PRESSED FOR CLT132 ***
      *
     C*** BEG CHG ***** 05/26/08 **************************************
     C**************     WHEN      *IN31 = '1' AND
     C**************               *IN07 = '0'
     C                   WHEN      (*IN31 = '1' AND *IN07 = '0')
     C                             OR *IN31 = '0'
     C*** END CHG ***** 05/26/08 **************************************
     C                   ITER
      *
     C                   OTHER
     C                   EVAL      *IN90 = '0'
     C*** BEG CHG ***** 05/26/08 **************************************
     C/EXEC SQL
     C+ DELETE FROM WFA2P WHERE A2MEDT = :@@MONENDDT
     C/END-EXEC
     C*** END CHG ***** 05/26/08 **************************************
     C                   LEAVE
      *
     C                   ENDSL
      *
     C                   ENDDO
      *
     C****               IF        *INKL = *ON
     C****               EVAL      *IN90 = *ON
     C****               EVAL      *INKL = *OFF
     C****               ENDIF
      *** END CHG ***** 01/24/07 **************************************
     C                   ENDIF

     C                   ENDIF

     C                   ENDSR
     *====================================================================
    * OTHFKS - Processing for other functions keys
     *====================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     *====================================================================
    * PGMEND - End of program processing
     *====================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     *====================================================================
    * PGMSTR - Used for any start of program processing (one-time)
     *====================================================================
     C     PGMSTR        BEGSR
    C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   IF        %PARMS >= 1 and @@PRDS <> *BLANKS
     C                   MOVEL(P)  @@PRDS        ##DATA
     C                   ENDIF

     C                   EVAL      SCPRDS = CENTERFLD(##DATA:%LEN(SCPRDS))
    
     C                   CLEAR                   XXCALL
     C                   IF        %PARMS >= 2
     C                   MOVEL     @@CALL        XXCALL
     C                   ENDIF
    
      *** Beg Chg 01/24/07 ********************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
     C                   SELECT
     C                   WHEN      CLTID = '132'
     C                   EVAL      *IN31 = '1'
     C                   OTHER
     C                   EVAL      *IN31 = '0'
     C                   ENDSL
      *
      *** End Chg 01/24/07 ********************************************
      *
     C                   ENDSR
     *====================================================================
    * PREDSP - Pre-processing before displaying (every time)
     *====================================================================
     C     PREDSP        BEGSR
     C                   ENDSR
     *====================================================================
    * PRSCTL - Process control after no errors found (Call Submitter)
     *====================================================================
     C     PRSCTL        BEGSR

     C                   CALLP     SUBMITTER(
     C                                       %EDITC(##CO#:'Z'):
     C                                       %EDITC(@MONENDDT:'Z')
     C                                      )
      *
     C                   ENDSR
