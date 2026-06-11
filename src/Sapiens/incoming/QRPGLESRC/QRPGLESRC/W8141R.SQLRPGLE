    HCOPYRIGHT('(C) Copyright Fiserv, Inc., 2008')
    H*********************************************************************
    H/TITLE A/R Reconciliation Work File Build
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DEBUG(*YES)
    H*********************************************************************
    H*  CALLED FROM: W8141C                                              *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*  CREATED: 11/22/06      C:183     PROJECT: 132WHL                 *
    H*********************************************************************
    H*  REVISIONS                                                        *
     H*                                                                   *
     H* 05/26/08 C:183 Support: WA132/63557                               *
     H*                Changed the logic to query PDSTST/PDSFIL           *
     H*                WFA2P rather than PDSRPT WFA2P.                    *
    H*                                                                   *
     H*  01/17/08  C:61  Project: 132WKD                                  *
     H*                - Correct commission calculations on Agency        *
    H*                  Billing transactions.                            *
    H*                - Filter out TEST group transactions.              *
    H*                                                                   *
     H*  05/01/07  C:183 Project: 132WHL                                  *
     H*                  Added A2MADJ and its population logic.           *
    H*                                                                   *
     H*  04/10/07  C:183 Project: 132WIA                                  *
     H*                  Changed the logic of population of A2DFFL,       *
     H*                  A2AMT1, A2AMT2, and A2AMTC.                      *
    H*                                                                   *
     H*  03/19/07  C:183 Project: 132WIA                                  *
     H*                  Changed the format of A2CYM from CYYMM to YYYYMM *
     H*                  and of the new fields A2ACDT and A2STMT from     *
    H*                  MMDDYYYY and MMYYYY to YYYYMMDD and YYYYMM       *
    H*                  respectively.                                    *
    H*                                                                   *
     H*  03/15/07  C:183 Project: 132WIA                                  *
     H*                  Added 2 new fields A2ACDT and A2STMT population  *
     H*                  logic. Also changed the A2DFFL population logic. *
     H*                                                                   *
    H*********************************************************************

     *********************************************************************
    *  File Specification                                               *
     *********************************************************************
      *
     FWFARP     IF   E             DISK
      *
     FWDABP     IF   E             DISK
      *
     FWMFNL     IF   E           K DISK
      *
     FWMAGL     IF   E           K DISK
      *
     FWMARL     IF   E           K DISK    RENAME(PAR01:WMARLR)
     F                                     PREFIX('X_')
      *
     FWMAHL     IF   E           K DISK
      *
     FWDELL     IF   E           K DISK
      *
     FWMRPL     IF   E           K DISK
      *
     FWMEML     IF   E           K DISK
      *
     FSMCOL     IF   E           K DISK
      *
     FWTABL     IF   E           K DISK
      *
     FWFA2P     UF A E             DISK
      *
     *********************************************************************
    *  Definition Specification                                         *
     *********************************************************************
      *
     D@LNAME           S             10
     D @CMD            S             50
     D @LEN            S             15P 5
     DSUFFIX           S              3
     D@RECCOUNT        S             10S 0 INZ(0)
     D @RPTDT          S              5  0
     D @RPDT#          S              5
      *** BEGIN ADD *** 03/19/07 **************************************
     D @MEDT           S              8
     D @ABDT           S              8
     D @ABSTDT         S              8
      *** END   ADD *** 03/19/07 **************************************
     D @@RPDT#         S              7
     D @@@RPDT#        S               D   DATFMT(*ISO)
     D @WTTTP          S              1
     D @A2IDT          S               D
     D @A2PODT         S              7  0
     D @DATE           S              7  0
     D @@A2PODT        S               D
     D @CURDT7         S              7  0
     D @TMPDT          S               D
     D @RRN            S              9  0
     D @A2AFE          S                   LIKE(A2AFE1)
     D @A2AMD          S                   LIKE(A2AMD1)
     D @AHRT#          S                   LIKE(AHRT#)
      *** BEGIN ADD *** 03/19/07 **************************************
     D SRDTI           S              8  0
     D SRDTO           S             10
     D SRERR           S              1
     D SRFFMT          S              8
     D SRTFMT          S              8
      *** END   ADD *** 03/19/07 **************************************
      *
      **********************************************************************
     *  Save Variable Declaration                                         *
      **********************************************************************
      *
     D SVCO#           S                   LIKE(A2CO#)
     D SVFND           S                   LIKE(A2FND)
     D SVEMP#          S                   LIKE(A2EMP#)
     D SVDIV           S                   LIKE(A2DIV)
     D SVFYR           S                   LIKE(A2FYR)
      *
     *********************************************************************
    *  Copybooks for Prototypes                                         *
     *********************************************************************
      *
     D/COPY SPRSTRING
     D/COPY SPRDATE
     D/COPY SPRCOMPANY
     D/COPY WPRUSRPRMS
     D/COPY WPRAGENT
     D/COPY WPRAR
     D/COPY WPRCHAPTER
     D/COPY WPROFFICE
     D/COPY WPRREGION
     D/COPY WPRPRODUCE
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     *********************************************************************
    *  Parameter List                                                   *
     *********************************************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    @CO#              3 0
     C                   PARM                    @RPDT#            5
      *
     *********************************************************************
    *  Key Lists                                                        *
     *********************************************************************
      *
     C     WMFNLKY       KLIST
     C                   KFLD                    A2CO#
     C                   KFLD                    A2FND
      *
     C     WMAGLKY       KLIST
     C                   KFLD                    A2CO#
     C                   KFLD                    @A2AFE
     C                   KFLD                    @A2AMD

     C     WMARLKY       KLIST
     C                   KFLD                    A2CO#
     C                   KFLD                    A2FND
     C                   KFLD                    @AHRT#
     C                   KFLD                    AHEFDT

     C     WMARLKY1      KLIST
     C                   KFLD                    A2CO#
     C                   KFLD                    A2FND
     C                   KFLD                    @AHRT#

     C     WMEMLKY       KLIST
     C                   KFLD                    ABCO#
     C                   KFLD                    ABFND
     C                   KFLD                    ABEMP#
     C                   KFLD                    ABDIV

     C     WMAHLKY       KLIST
     C                   KFLD                    A2CO#
     C                   KFLD                    A2FND
     C                   KFLD                    A2EMP#
     C                   KFLD                    A2DIV
     C                   KFLD                    A2FYR
     C                   KFLD                    @DATE

     C     WMAHLKY1      KLIST
     C                   KFLD                    A2CO#
     C                   KFLD                    A2FND
     C                   KFLD                    A2EMP#
     C                   KFLD                    A2DIV
     C                   KFLD                    A2FYR

     C     WDELLKY       KLIST
     C                   KFLD                    A2CO#
     C                   KFLD                    A2FND
     C                   KFLD                    A2EMP#
     C                   KFLD                    A2DIV
     C                   KFLD                    A2FYR
      *
     C     WTABLKY       KLIST
     C                   KFLD                    A2CO#
     C                   KFLD                    XXKEY            10
      *
      *** BEGIN ADD *** 03/19/07 **************************************
     C     DATCHK        PLIST
     C                   PARM                    SRDTI
     C                   PARM                    SRFFMT
     C                   PARM                    SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *** END   ADD *** 03/19/07 **************************************
     *********************************************************************
     *  Main Line of Code                                                *
     *********************************************************************
      *
      * To construct the name of the library.
      *
     C                   CALL      'SA030R'
     C                   PARM                    SUFFIX
      *
     C*** BEG DEL ***** 05/26/08 **************************************
     C**************     EVAL      @LNAME = 'PDSRPT' + SUFFIX
      *
     C**************     EVAL      @CMD = 'ADDLIBLE LIB(' + @LNAME + ')'
      *
      * Add the PDSRPTXXX Library in the Library List *
      *
     C**************     CALL      'QCMDEXC'
     C**************     PARM                    @CMD
     C**************     PARM      50            @LEN
     C*** END DEL ***** 05/26/08 **************************************
      *
      ***  Delete the month-end data, if present, from PDSRPTXXX/WFA2PSAV ***
      *
      *** BEGIN CHG *** 03/19/07 **************************************
     C                   MOVE      @RPDT#        @@@MEDT           5 0
     C                   EVAL      @MEDT = %EDITC(DT6MDYY(@@@MEDT):'X')
     C                   EVAL      @MEDT='01'+ @MEDT
      *
     C                   EVAL      SRFFMT = '*DMYY'
     C                   EVAL      SRTFMT = '*YYMD'
     C                   MOVE      @MEDT         SRDTI
      *
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C                   MOVEL     SRDTO         @@MEDT            6 0
      *** END   CHG *** 03/19/07 **************************************
     C*** BEG DEL ***** 05/26/08 **************************************
     C*/EXEC SQL
     C*** END DEL ***** 05/26/08 **************************************
      *** BEGIN CHG *** 03/19/07 **************************************
     C*+ DELETE FROM WFA2PSAV WHERE A2CYM = :@RPDT#
     C*** BEG DEL ***** 05/26/08 **************************************
     C*+ DELETE FROM WFA2P WHERE A2MEDT = :@@MEDT
     C*** END DEL ***** 05/26/08 **************************************
      *** END   CHG *** 03/19/07 **************************************
     C*** BEG DEL ***** 05/26/08 **************************************
     C*/END-EXEC
     C*** END DEL ***** 05/26/08 **************************************
      *
     C*** BEG DEL ***** 05/26/08 **************************************
     C***************    EVAL      @CMD = 'RMVLIBLE LIB(' + @LNAME + ')'
      *
     C***************    CALL      'QCMDEXC'
     C***************    PARM                    @CMD
     C***************    PARM                    @LEN
     C*** END DEL ***** 05/26/08 **************************************
      *****************************************************************
      * Read WFARP and then populate fields through ARTOA2 subroutine *
      *****************************************************************
     C                   DOW       NOT %EOF(WFARP)
      *
     C                   READ      WFARP
      *
     C                   IF        %EOF(WFARP)
     C                   LEAVE
     C                   ENDIF
      *
     C                   EXSR      ARTOA2
      ***** beg add ******* 01/17/08 **********************************

     c                   if        fntest <> 'Y'
      ***** end add ******* 01/17/08 **********************************
     C                   WRITE     PA201
      ***** beg add ******* 01/17/08 **********************************
     c                   endif
      ***** end add ******* 01/17/08 **********************************
      *
     C                   EXSR      WFTOSAVF
      *
     C                   ENDDO
      *
     C                   EXSR      RSTSAVF
      *
     C                   EXSR      REFSECAGT
      *
      *****************************************************************
      * Read WDABP and then populate fields through WDTOA2 subroutine *
      *****************************************************************
      *
     C                   DOW       NOT %EOF(WDABP)
      *
     C                   READ      WDABP
      *
     C                   IF        %EOF(WDABP)
     C                   LEAVE
     C*                  ELSEIF    ABEMP# = 0
     C*                  ITER
     C                   ENDIF
      *
     C                   EXSR      WDTOA2
      *
      ***** beg add ******* 01/17/08 **********************************

     c                   if        fntest <> 'Y'
      ***** end add ******* 01/17/08 **********************************
     C                   WRITE     PA201
      ***** beg add ******* 01/17/08 **********************************
     c                   endif
      ***** end add ******* 01/17/08 **********************************
      *
     C                   EXSR      WDTOSAVF
      *
     C                   ENDDO
      *
     C                   EVAL      *INLR = *ON
      *
     *====================================================================
    *  ARTOA2 - WFARP to WFA2P                                          *
     *====================================================================
      *
     C     ARTOA2        BEGSR
      *
     C                   EVAL      A2CO# = ARCO#
     C                   EVAL      A2CONM = ARCNAM
     C                   EVAL      A2FND = ARFND
     C                   EVAL      A2FNNM = ARGNAM
      *
     C                   IF        A2FND <> SVFND
     C                   EVAL      A2FSNM = ' '
     C     WMFNLKY       CHAIN     WMFNL
     C                   IF        %FOUND(WMFNL)
     C                   EVAL      A2FSNM = FNSHNM
     C                   ELSE
     C                   EVAL      A2FSNM = ' '
     C                   ENDIF
     C                   ENDIF
      *
     C                   EVAL      A2EMP# = AREMP#
     C                   EVAL      A2DIV = ARDIV
     C                   EVAL      A2EMNM = ARENAM
     C                   EVAL      A2FYR = ARFYR
      *
     C                   IF        ARCO# <> SVCO# OR ARFND <> SVFND OR
     C                             AREMP# <> SVEMP# OR ARDIV <> SVDIV OR
     C                             ARFYR <> SVFYR
     C                   EVAL      A2BSCD = 0
     C                   EVAL      A2SEFL = ' '
     C     WDELLKY       CHAIN     WDELL
     C                   IF        %FOUND(WDELL)
     C                   EVAL      A2BSCD = ELBSCD
     C                   ELSE
     C                   EVAL      A2BSCD = 0
     C                   ENDIF
      *
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     WTABLKY       CHAIN     WTABL
     C                   IF        %FOUND(WTABL)
     C                   EVAL      A2SEFL = BSSEFL
     C                   ELSE
     C                   EVAL      A2SEFL = ' '
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   EVAL      A2PFDT = ARPFDT
     C                   EVAL      A2PTDT = ARPTDT
     C                   EVAL      A2CXDT = ARCXDT
     C                   EVAL      A2CAP# = ARPOL#
     C                   EVAL      A2TPFL = 'D'
      *
     C                   EVAL      A2DFFL = ' '
     C                   MOVE      @RPDT#        @RPTDT
      *
     *====================================================================
      * Logic for Comparison of ARPODT and Report Date
     *====================================================================
      *
     C                   EVAL      @@RPDT# = @RPDT# +'01'
     C                   EVAL      @@@RPDT# = GETLASTDAY(%DATE(@@RPDT#:*CYMD0))
      *** BEGIN CHG *** 03/15/07 **************************************
     C****               IF        ARPODT <> 0
     C****               IF        %DATE(ARPODT:*ISO) > @@@RPDT#
     C****               EVAL      A2DFFL = 'Y'
     C****               ELSE
     C****               EVAL      A2DFFL = ' '
     C****               ENDIF
     C****               ENDIF
      *** BEGIN CHG *** 04/10/07 **************************************
     C****               IF        (ARARTY = 'P' OR ARARTY = 'I' OR
     C****                         ARARTY = 'Q' OR ARARTY = 'R' OR
     C****                         ARARTY = 'T' OR ARARTY = 'V') AND
     C****                         ARTRDT <> 0
     C                   IF        (ARARTY = 'P' OR ARARTY = 'I' OR
     C                              ARARTY = 'Q' OR ARARTY = 'V' OR
     C                              ARARTY = 'N' OR ARARTY = 'R' OR
     C                              ARARTY = 'T') AND
     C                              ARTRDT <> 0
     C*                  IF        %DATE(ARTRDT:*ISO) >= @@@RPDT#
     C                   IF        %DATE(ARTRDT:*ISO) > @@@RPDT#
      *** END   CHG *** 04/10/07 **************************************
     C                   EVAL      A2DFFL = 'Y'
     C                   ELSE
     C                   EVAL      A2DFFL = ' '
     C                   ENDIF
     C                   ELSEIF    ARPODT <> 0
      *** BEGIN CHG *** 04/10/07 **************************************
     C*                  IF        %DATE(ARPODT:*ISO) >= @@@RPDT#
     C                   IF        %DATE(ARPODT:*ISO) > @@@RPDT#
      *** END   CHG *** 04/10/07 **************************************
     C                   EVAL      A2DFFL = 'Y'
     C                   ELSE
     C                   EVAL      A2DFFL = ' '
     C                   ENDIF
     C                   ENDIF
      *** END   CHG *** 03/15/07 **************************************
      *
     C                   EVAL      @WTTTP = GETARCOMM(ARARCD:@RRN:A2CO#)
      *** BEGIN CHG *** 04/10/07 **************************************
     C****               IF        (ARARTY = 'B' OR ARARTY = 'A' OR
     C****                         ARARTY = 'D' OR ARARTY = 'F' OR
     C****                         ARARTY = 'H') AND @WTTTP = ' '
     C****               EVAL      A2CMFL = 'Y'
     C****               ELSE
     C****               EVAL      A2CMFL = ' '
     C****               ENDIF
      *** END   CHG *** 04/10/07 **************************************
      *
     C                   EVAL      A2ARCD = ARARCD
     C                   EVAL      A2ARTY = ARARTY
     C                   EVAL      A2CTTY = ARCTYP
     C                   EVAL      A2DESC = ARDESC
      *
     C                   EVAL      A2QTFL = 'L'
      *
     C                   EVAL      A2AMT = ARAMT
     C                   EVAL      A2CLAM = ARCOLL
     C                   EVAL      A2BLAM = ARBILL
     C                   EVAL      A2OTAM = AROTHR
     C                   EVAL      A2TRDT = ARTRDT
     C                   EVAL      A2PODT = ARPODT
     C                   EVAL      A2CHK# = ARCHK#
     C                   EVAL      A2INV# = ARNINV
     C                   EVAL      A2LTFL = ARLATE
     C                   EVAL      A2RTST = ARRST
     C                   EVAL      A2RGN# = ARRGN#
     C                   EVAL      A2RGDS = ARRGDS
     C                   EVAL      A2CHAP = ARCHAP
     C                   EVAL      A2CHDS = ARCDES
     C                   EVAL      A2OFC# = AROFF#
     C                   EVAL      A2OFNM = AROFNM
     C                   EVAL      A2PFCO = ARPFIN
     C                   EVAL      A2PFNM = ARFNAM
     C                   EVAL      A2SRFL = ARSUR
      *
     C                   EXSR      FIRSTAGNT
      *
     C                   IF        A2AGT2 <> 0
     C                   EXSR      SECONDAGNT
     C                   ELSE
     C                   EXSR      REFSECAGT
     C                   ENDIF
      *** BEGIN CHG *** 04/10/07 **************************************
     C                   IF        A2AMT1 <> 0 OR A2AMT2 <> 0
     C                   EVAL      A2CMFL = 'Y'
     C                   ELSE
     C                   EVAL      A2CMFL = ' '
     C                   ENDIF
      *** END   CHG *** 04/10/07 **************************************
      *
     C                   EXSR      CURRAGNT
      *
     C                   EVAL      A2IPGM = Q1PGM
     C                   EVAL      A2IUSR = Q1USER
      *
     C                   EVAL      @A2IDT = SYSTEMDATE()
     C     *ISO          MOVE      @A2IDT        A2IDT
     C                   EVAL      A2TIME = TIMETO6(SYSTEMTIME())
      *** BEGIN CHG *** 03/19/07 **************************************
     C*                  EVAL      A2CYM = @RPTDT
     C                   EVAL      A2MEDT= @@MEDT
      *** END   CHG *** 03/19/07 **************************************
      *
     C                   EVAL      @RECCOUNT = @RECCOUNT + 1
     C                   EVAL      A2SEQ# = @RECCOUNT
      *
      *** BEGIN ADD *** 05/01/07 **************************************
     C                   EVAL      A2MADJ = ' '
      *** END   ADD *** 05/01/07 **************************************

     C                   ENDSR
      *
     *====================================================================
    *  WDTOA2 - WDABP to WFA2P                                          *
     *====================================================================
      *
     C     WDTOA2        BEGSR
      *
     C                   EVAL      A2CO# = ABCO#
      *
     C     ABCO#         CHAIN     SMCOL
     C                   IF        %FOUND(SMCOL)
     C                   EVAL      A2CONM = CONAME
     C                   ELSE
     C                   EVAL      A2CONM = ' '
     C                   ENDIF
      *
     C                   EVAL      A2FND = ABFND
     C                   IF        ABFND <> SVFND
     C     WMFNLKY       CHAIN     WMFNL
     C                   IF        %FOUND(WMFNL)
     C                   EVAL      A2FNNM = FNNAME
     C                   EVAL      A2FSNM = FNSHNM
     C                   ELSE
     C                   EVAL      A2FNNM = ' '
     C                   EVAL      A2FSNM = ' '
     C                   ENDIF
     C                   ENDIF
      *
     C                   EVAL      A2EMP# = ABEMP#
     C                   EVAL      A2DIV = ABDIV
      *
     C                   IF        ABEMP# <> SVEMP# OR ABDIV <> SVDIV
     C     WMEMLKY       CHAIN     WMEML
     C                   IF        %FOUND(WMEML)
     C                   EVAL      A2EMNM = EMNAME
      *** Begin Add *** 03/19/07 **************************************
     C                   IF        EMCAD1 <> 0
      *** End   Add *** 03/19/07 **************************************
     C                   EVAL      @TMPDT = CVTTODATE(EMCAD1)
     C     *ISO          MOVE      @TMPDT        A2CXDT
      *** Begin Add *** 03/19/07 **************************************
     C                   ELSE
     C                   EVAL      A2CXDT = 0
     C                   ENDIF
      *** End   Add *** 03/19/07 **************************************
     C                   EVAL      A2RGN# = EMRGN#
     C                   EVAL      A2OFC# = EMOFF#
     C                   ELSE
     C                   EVAL      A2EMNM = ' '
     C                   EVAL      A2CXDT = 0
     C                   EVAL      A2RGN# = 0
     C                   EVAL      A2OFC# = ' '
     C                   ENDIF
     C                   ENDIF

     C                   EVAL      A2FYR = ABFYR
      *
     C                   IF        ABCO# <> SVCO# OR ABFND <> SVFND OR
     C                             ABEMP# <> SVEMP# OR ABDIV <> SVDIV OR
     C                             ABFYR <> SVFYR
     C                   EVAL      A2BSCD = 0
     C                   EVAL      A2SEFL = ' '
     C     WDELLKY       CHAIN     WDELL
     C                   IF        %FOUND(WDELL)
     C                   EVAL      A2BSCD = ELBSCD
     C                   ELSE
     C                   EVAL      A2BSCD = 0
     C                   ENDIF
      *
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     WTABLKY       CHAIN     WTABL
     C                   IF        %FOUND(WTABL)
     C                   EVAL      A2SEFL = BSSEFL
     C                   ELSE
     C                   EVAL      A2SEFL = ' '
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   IF        ABCO# <> SVCO# OR ABFND <> SVFND OR
     C                             ABEMP# <> SVEMP# OR ABDIV <> SVDIV OR
     C                             ABFYR <> SVFYR
     C     WDELLKY       CHAIN     WDELL
     C                   IF        %FOUND(WDELL)
     C                   EVAL      @TMPDT = CVTTODATE(ELPFDT)
     C     *ISO          MOVE      @TMPDT        A2PFDT
     C                   EVAL      @TMPDT = CVTTODATE(ELPTDT)
     C     *ISO          MOVE      @TMPDT        A2PTDT
     C                   ELSE
     C                   EVAL      A2PFDT = 0
     C                   EVAL      A2PTDT = 0
     C                   ENDIF
     C     WDELLKY       CHAIN     WMRPL
     C                   IF        %FOUND(WMRPL)
     C                   EVAL      A2CAP# = RPCAP#
     C                   ELSE
     C                   EVAL      A2CAP# = ' '
     C                   ENDIF
     C                   ENDIF
      *
     C                   EVAL      A2TPFL = 'A'
      *
     C                   EVAL      A2DFFL = ' '
     C                   MOVE      @RPDT#        @RPTDT
      *
     C                   EVAL      @@RPDT# = @RPDT# +'01'
     C                   EVAL      @@@RPDT# = GETLASTDAY(%DATE(@@RPDT#:*CYMD0))
      *** BEGIN CHG *** 03/15/07 **************************************
     C****               IF        CVTTODATE(ABPODT) > @@@RPDT#
     C****               EVAL      A2DFFL = 'Y'
     C****               ELSE
     C****               EVAL      A2DFFL = ' '
     C****               ENDIF
      *** BEGIN CHG *** 04/10/07 **************************************
     C****               IF        (ABARTY = 'P' OR ABARTY = 'I' OR
     C****                         ABARTY = 'Q' OR ABARTY = 'R' OR
     C****                         ABARTY = 'T' OR ABARTY = 'V') AND
     C****                         ABTRDT <> 0
     C****               IF        CVTTODATE(ABTRDT) >= @@@RPDT#
     C                   IF        (ABARTY = 'P' OR ABARTY = 'I' OR
     C                              ABARTY = 'Q' OR ABARTY = 'V' OR
     C                              ABARTY = 'N' OR ABARTY = 'R' OR
     C                              ABARTY = 'T') AND
     C                             ABTRDT <> 0
     C                   IF        CVTTODATE(ABTRDT) > @@@RPDT#
      *** END   CHG *** 04/10/07 **************************************
     C                   EVAL      A2DFFL = 'Y'
     C                   ELSE
     C                   EVAL      A2DFFL = ' '
     C                   ENDIF
     C                   ELSEIF    ABPODT <> 0
      *** BEGIN CHG *** 04/10/07 **************************************
     C*                  IF        CVTTODATE(ABPODT) >= @@@RPDT#
     C                   IF        CVTTODATE(ABPODT) > @@@RPDT#
      *** END   CHG *** 04/10/07 **************************************
     C                   EVAL      A2DFFL = 'Y'
     C                   ELSE
     C                   EVAL      A2DFFL = ' '
     C                   ENDIF
     C                   ENDIF
      *** END   CHG *** 03/15/07 **************************************
      *
     C                   EVAL      @WTTTP = GETARCOMM(ABARCD:@RRN:A2CO#)
      *** BEGIN CHG *** 04/10/07 **************************************
     C****               IF        (ABARTY = 'B' OR ABARTY = 'A' OR
     C****                         ABARTY = 'D' OR ABARTY = 'F' OR
     C****                         ABARTY = 'H') AND @WTTTP = ' '
     C****               EVAL      A2CMFL = 'Y'
     C****               ELSE
     C****               EVAL      A2CMFL = ' '
     C****               ENDIF
      *** END   CHG *** 04/10/07 **************************************
      *
     C                   EVAL      A2ARCD = ABARCD
     C                   EVAL      A2ARTY = ABARTY
      *
     C                   EVAL      A2CTTY = GETARCAT(ABARCD:@RRN:A2CO#)
      *
     C                   EVAL      A2DESC = ABDESC
      *
     C                   EVAL      A2QTFL = 'L'
      *
     C                   EVAL      A2AMT = ARVALUE(ABAMT:ABARTY)
      *
     C                   IF        ABARTY = 'P' OR ABARTY = 'Q' OR
     C                             ABARTY = 'I' OR ABARTY = 'R' OR
     C                             ABARTY = 'V' OR ABARTY = 'N' OR
     C                             ABARTY = 'T'
     C                   EVAL      A2CLAM = ABAMT
     C                   ELSE
     C                   EVAL      A2CLAM = 0
     C                   ENDIF
      *
     C                   IF        ABARTY = 'B' OR ABARTY = 'F' OR
     C                             ABARTY = 'H' OR ABARTY = 'A' OR
     C                             ABARTY = 'D'
     C                   EVAL      A2BLAM = ABAMT
     C                   ELSE
     C                   EVAL      A2BLAM = 0
     C                   ENDIF
      *
     C                   EVAL      A2OTAM = 0
     C                   EVAL      @TMPDT = CVTTODATE(ABTRDT)
     C     *ISO          MOVE      @TMPDT        A2TRDT
     C                   EVAL      @TMPDT = CVTTODATE(ABPODT)
     C     *ISO          MOVE      @TMPDT        A2PODT
     C                   EVAL      A2CHK# = ABCHK#
     C                   EVAL      A2INV# = 0
     C                   EVAL      A2LTFL = *BLANK
     C                   EVAL      A2RTST = ABRST
      *
     C                   EVAL      A2RGDS = GETRGNDESC(A2RGN#:@RRN:A2FND:A2CO#)
      *
     C                   EVAL      A2CHAP = ABCHAP
      *
     C                   EVAL      A2CHDS = GETCHAPDES(A2CHAP:@RRN:A2FND:A2CO#)
      *
     C                   EVAL      A2OFNM = GETOFFNAME(A2OFC#:@RRN:A2CO#)
      *
     C                   EVAL      A2PFCO = *BLANKS
     C                   EVAL      A2PFNM = *BLANKS
      *
     C                   IF        A2CTTY = '1' OR A2CTTY = '2' OR
     C                             A2CTTY = '3' OR A2CTTY = '4' OR
     C                             A2CTTY = '5' OR A2CTTY = 'T'
     C                   EVAL      A2SRFL = 'Y'
     C                   ELSE
     C                   EVAL      A2SRFL = ' '
     C                   ENDIF
      *
     C                   EXSR      FIRSTAGNT
      *
     C                   EVAL      A2AGT2 = 0
     C                   EVAL      A2ARC2 = 0
     C                   EVAL      A2APR2 = 0
      *** BEGIN CHG *** 04/10/07 **************************************
     C                   IF        A2AMT1 <> 0
     C                   EVAL      A2CMFL = 'Y'
     C                   ELSE
     C                   EVAL      A2CMFL = ' '
     C                   ENDIF
      *** END   CHG *** 04/10/07 **************************************
      *
     C                   EXSR      CURRAGNT
     C*** BEGIN ADD *** 03/15/07 **************************************
      *
      *** BEGIN CHG *** 03/19/07 **************************************
     C                   EVAL      SRFFMT = '*MDYY'
     C                   EVAL      SRTFMT = '*YYMD'
      *
     C                   EVAL      @ABDT = %EDITC(DTMDYY(ABDT):'X')
     C                   MOVE      @ABDT         SRDTI
      *
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C                   MOVEL     SRDTO         A2ACDT
     C*                  EVAL      A2ACDT = DTMDYY(ABDT)
      *
     C                   EVAL      SRFFMT = '*DMYY'
     C                   EVAL      SRTFMT = '*YYMD'
      *
     C                   EVAL      @ABSTDT = '01'+%EDITC(DT6MDYY(ABSTDT):'X')
     C                   MOVE      @ABSTDT       SRDTI
      *
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C                   MOVEL     SRDTO         A2STDT
     C*                  EVAL      A2STMT = DT6MDYY(ABSTDT)
      *** END   CHG *** 03/19/07 **************************************
      *
     C*** END   ADD *** 03/15/07 **************************************
      *** BEGIN ADD *** 05/01/07 **************************************
     C                   EVAL      A2MADJ = ABMADJ
      *** END   ADD *** 05/01/07 **************************************
      *
     C                   EVAL      A2IPGM = Q1PGM
     C                   EVAL      A2IUSR = Q1USER
     C                   EVAL      @A2IDT = SYSTEMDATE()
     C     *ISO          MOVE      @A2IDT        A2IDT
     C                   EVAL      A2TIME = TIMETO6(SYSTEMTIME())
      *** BEGIN CHG *** 03/19/07 **************************************
     C*                  EVAL      A2CYM = @RPTDT
     C                   EVAL      A2MEDT= @@MEDT
      *** END   CHG *** 03/19/07 **************************************
      *
     C                   EVAL      @RECCOUNT = @RECCOUNT + 1
     C                   EVAL      A2SEQ# = @RECCOUNT
      *
     C                   ENDSR
      *
     *====================================================================
    *  WDTOSAVF - WDABP to Save Fields, used for Breaks                 *
     *====================================================================
      *
     C     WDTOSAVF      BEGSR
      *
     C                   EVAL      SVCO# = ABCO#
     C                   EVAL      SVFND = ABFND
     C                   EVAL      SVEMP# = ABEMP#
     C                   EVAL      SVDIV = ABDIV
     C                   EVAL      SVFYR = ABFYR
      *
     C                   ENDSR
      *
     *====================================================================
    *  WFTOSAVF - WFARP to Save Fields, used for Breaks                 *
     *====================================================================
      *
     C     WFTOSAVF      BEGSR
      *
     C                   EVAL      SVCO# = ARCO#
     C                   EVAL      SVFND = ARFND
     C                   EVAL      SVEMP# = AREMP#
     C                   EVAL      SVDIV = ARDIV
     C                   EVAL      SVFYR = ARFYR
      *
     C                   ENDSR
      *
     *====================================================================
    *  FIRSTAGNT - Set First Agent Fields                               *
     *====================================================================
      *
     C     FIRSTAGNT     BEGSR
     C                   EVAL      A2AGT1 = 0
     C                   EVAL      A2ARC1 = 0
     C                   EVAL      A2APR1 = 0
     C                   EVAL      A2AGT2 = 0
     C                   EVAL      A2ARC2 = 0
     C                   EVAL      A2APR2 = 0
      *
      *========================================================
      * Preparation of 7 digit date for Keying WMAHL1         *
      *========================================================
      *
     C                   IF        A2PODT <> 0
      *
     C                   MOVE      A2PODT        @@A2PODT
     C                   EVAL      @DATE = DTCYMD(DATETO8(@@A2PODT))
      *
     C     WMAHLKY       SETGT     WMAHL
     C     WMAHLKY1      READPE    WMAHL                                  35
      *
     C                   DOW       NOT %EOF(WMAHL)
     C                   IF        AHSTAT = 'A'
     C                   EVAL      A2AGT1 = AHAGT#
     C                   EVAL      A2ARC1 = AHRT#
     C                   EVAL      A2APR1 = AHPROD
     C                   EVAL      A2AGT2 = AHSAGT
     C                   EVAL      A2ARC2 = AHSRT#
     C                   EVAL      A2APR2 = AHSPRD
     C                   LEAVE
     C                   ELSE
     C                   EVAL      A2AGT1 = 0
     C                   EVAL      A2ARC1 = 0
     C                   EVAL      A2APR1 = 0
     C                   EVAL      A2AGT2 = 0
     C                   EVAL      A2ARC2 = 0
     C                   EVAL      A2APR2 = 0
     C                   ENDIF
     C     WMAHLKY1      READPE    WMAHL                                  35
     C                   ENDDO
      *
     C                   ENDIF
      *
     C                   IF        A2AGT1 <> 0
      *
     C                   EVAL      A2ANM1 = GETAGTNAME(A2AGT1:@RRN:@RRN: +
     C                             A2FND:A2CO#)
      *
     C                   EVAL      A2AFE1 = GETAGTFEIN(A2CO#:A2FND:A2AGT1: +
     C                             A2AMD1)
      *
     C                   EVAL      @A2AFE = A2AFE1
     C                   EVAL      @A2AMD = A2AMD1
      *
     C     WMAGLKY       CHAIN     WMAGL
     C                   IF        %FOUND(WMAGL)
     C                   EVAL      A2AAD1 = AGMAD1
     C                   EVAL      A2ACT1 = AGMCTY
     C                   EVAL      A2AST1 = AGMST
     C                   EVAL      A2AZP1 = AGMZIP
     C                   ELSE
     C                   EVAL      A2AAD1 = ' '
     C                   EVAL      A2ACT1 = ' '
     C                   EVAL      A2AST1 = ' '
     C                   EVAL      A2AZP1 = 0
     C                   ENDIF
      *
     C                   EVAL      @AHRT# = A2ARC1
     C                   EVAL      A2ARD1 = ' '
     C                   EVAL      A2ARV1 = 0
      *
     C     WMARLKY       SETGT     WMARL
     C     WMARLKY1      READPE    WMARL                                  32
      *
     C                   DOW       NOT %EOF(WMARL)
     C                   IF        X_ARSTAT = 'A'
     C                   EVAL      A2ARD1 = X_ARDESC
     C                   EVAL      A2ARV1 = X_ARNWRT
     C                   LEAVE
     C                   ELSE
     C                   EVAL      A2ARD1 = ' '
     C                   EVAL      A2ARV1 = 0
     C                   ENDIF
     C     WMARLKY1      READPE    WMARL                                  32
     C                   ENDDO
      *
     C                   EVAL      A2APN1 = GETPRODNM(A2AFE1:A2AMD1:A2APR1: +
     C                             @RRN:A2CO#)
      *
      ***** BEG DEL ******* 01/17/08 **********************************
      ****BEGIN ADD *** 04/10/07 **************************************
     C*********          IF        PAYCOMM(A2CO#:A2ARCD) = *ON
      ****END** ADD *** 04/10/07 **************************************
     C*********          EVAL(H)   A2AMT1 = A2AMT * A2ARV1 / 100
      ****BEGIN ADD *** 04/10/07 **************************************
     C*********          ELSE
     C*********          EVAL      A2AMT1 = 0
     C*********          ENDIF
      ***** END DEL ******* 01/17/08 **********************************
      ***** BEG ADD ******* 01/17/08 **********************************

     c                   if        paycomm(a2co#:a2arcd) = *ON

     c                   if        a2tpfl = 'A'
     c                   eval      a2amt1 = abcomm

     c                   else

     c                   eval(h)   a2amt1 = a2amt * a2arv1 / 100
     c                   endif

     c                   else

     c                   eval      a2amt1 = 0
     c                   endif

      ***** END ADD ******* 01/17/08 **********************************
      *** END   ADD *** 04/10/07 **************************************
     C                   EVAL      A2NET1 = A2AMT - A2AMT1
      *
     C                   ELSE
      *
     C                   EXSR      REFFIRAGT
      *
     C                   ENDIF
      *
     C                   ENDSR
      *
     *====================================================================
    *  SECONDAGNT - Set Second Agent Fields                             *
     *====================================================================
      *
     C     SECONDAGNT    BEGSR
      *
     C                   EVAL      A2ANM2 = GETAGTNAME(A2AGT2:@RRN:@RRN: +
     C                             A2FND:A2CO#)
      *
     C                   EVAL      A2AFE2 = GETAGTFEIN(A2CO#:A2FND:A2AGT2: +
     C                             A2AMD2)
      *
     C                   EVAL      @A2AFE = A2AFE2
     C                   EVAL      @A2AMD = A2AMD2
      *
     C     WMAGLKY       CHAIN     WMAGL
     C                   IF        %FOUND(WMAGL)
     C                   EVAL      A2AAD2 = AGMAD1
     C                   EVAL      A2ACT2 = AGMCTY
     C                   EVAL      A2AST2 = AGMST
     C                   EVAL      A2AZP2 = AGMZIP
     C                   ELSE
     C                   EVAL      A2AAD2 = ' '
     C                   EVAL      A2ACT2 = ' '
     C                   EVAL      A2AST2 = ' '
     C                   EVAL      A2AZP2 = 0
     C                   ENDIF
      *
     C                   EVAL      @AHRT# = A2ARC2
     C                   EVAL      A2ARD2 = ' '
     C                   EVAL      A2ARV2 = 0
      *
     C     WMARLKY       SETGT     WMARL
     C     WMARLKY1      READPE    WMARL                                  32
      *
     C                   DOW       NOT %EOF(WMARL)
     C                   IF        X_ARSTAT = 'A'
     C                   EVAL      A2ARD2 = X_ARDESC
     C                   EVAL      A2ARV2 = X_ARNWRT
     C                   LEAVE
     C                   ELSE
     C                   EVAL      A2ARD2 = ' '
     C                   EVAL      A2ARV2 = 0
     C                   ENDIF
     C     WMARLKY1      READPE    WMARL                                  32
     C                   ENDDO
      *
     C                   EVAL      A2APN2 = GETPRODNM(A2AFE2:A2AMD2:A2APR2: +
     C                             @RRN:A2CO#)
      *
      *** BEGIN ADD *** 04/10/07 **************************************
     C                   IF        PAYCOMM(A2CO#:A2ARCD) = *ON
      *** END   ADD *** 04/10/07 **************************************
     C                   EVAL(H)   A2AMT2 = A2AMT * A2ARV2 / 100
      *** BEGIN ADD *** 04/10/07 **************************************
     C                   ELSE
     C                   EVAL      A2AMT2 = 0
     C                   ENDIF
      *** END   ADD *** 04/10/07 **************************************
     C                   EVAL      A2NET2 = A2AMT - A2AMT2
      *
     C                   ENDSR
     *====================================================================
    *  CURRAGNT - Set Current Agent Fields                              *
     *====================================================================
      *
     C     CURRAGNT      BEGSR
      *
     C                   EVAL      A2AGTC = 0
     C                   EVAL      A2ARCC = 0
     C                   EVAL      A2APRC = 0
      *
      *========================================================
      * Preparation of 7 digit date for Keying WMAHL1         *
      *========================================================
      *
     C                   EVAL      @DATE = DATETO7(SYSTEMDATE)
      *
     C     WMAHLKY       SETGT     WMAHL
     C     WMAHLKY1      READPE    WMAHL                                  35
      *
     C                   DOW       NOT %EOF(WMAHL)
     C                   IF        AHSTAT = 'A'
     C                   EVAL      A2AGTC = AHAGT#
     C                   EVAL      A2ARCC = AHRT#
     C                   EVAL      A2APRC = AHPROD
     C                   LEAVE
     C                   ELSE
     C                   EVAL      A2AGTC = 0
     C                   EVAL      A2ARCC = 0
     C                   EVAL      A2APRC = 0
     C                   ENDIF
     C     WMAHLKY1      READPE    WMAHL                                  35
     C                   ENDDO
      *
     C                   IF        A2AGTC <> 0
      *
     C                   EVAL      A2ANMC = GETAGTNAME(A2AGTC:@RRN:@RRN: +
     C                             A2FND:A2CO#)
      *
     C                   EVAL      A2AFEC = GETAGTFEIN(A2CO#:A2FND:A2AGTC: +
     C                             A2AMDC)
      *
     C                   EVAL      @A2AFE = A2AFEC
     C                   EVAL      @A2AMD = A2AMDC
      *
     C     WMAGLKY       CHAIN     WMAGL
     C                   IF        %FOUND(WMAGL)
     C                   EVAL      A2AADC = AGMAD1
     C                   EVAL      A2ACTC = AGMCTY
     C                   EVAL      A2ASTC = AGMST
     C                   EVAL      A2AZPC = AGMZIP
     C                   ELSE
     C                   EVAL      A2AADC = ' '
     C                   EVAL      A2ACTC = ' '
     C                   EVAL      A2ASTC = ' '
     C                   EVAL      A2AZPC = 0
     C                   ENDIF
      *
     C                   EVAL      @AHRT# = A2ARCC
     C                   EVAL      A2ARDC = ' '
     C                   EVAL      A2ARVC = 0
      *
     C     WMARLKY       SETGT     WMARL
     C     WMARLKY1      READPE    WMARL                                  32
      *
     C                   DOW       NOT %EOF(WMARL)
     C                   IF        X_ARSTAT = 'A'
     C                   EVAL      A2ARDC = X_ARDESC
     C                   EVAL      A2ARVC = X_ARNWRT
     C                   LEAVE
     C                   ELSE
     C                   EVAL      A2ARDC = ' '
     C                   EVAL      A2ARVC = 0
     C                   ENDIF
     C     WMARLKY1      READPE    WMARL                                  32
     C                   ENDDO
      *
     C                   EVAL      A2APNC = GETPRODNM(A2AFE1:A2AMD1:A2APR1: +
     C                             @RRN:A2CO#)
      *
      *** BEGIN ADD *** 04/10/07 **************************************
     C                   IF        PAYCOMM(A2CO#:A2ARCD) = *ON
      *** END   ADD *** 04/10/07 **************************************
     C                   EVAL(H)   A2AMTC = A2AMT * A2ARVC / 100
      *** BEGIN ADD *** 04/10/07 **************************************
     C                   ELSE
     C                   EVAL      A2AMTC = 0
     C                   ENDIF
      *** END   ADD *** 04/10/07 **************************************
     C                   EVAL      A2NETC = A2AMT - A2AMTC
      *
     C                   ELSE
      *
     C                   EXSR      REFCURAGT
      *
     C                   ENDIF
      *
     C                   ENDSR
      *
     *====================================================================
    *  RSTSAVF - Reset Save Fields                                     *
     *====================================================================
      *
     C     RSTSAVF       BEGSR
      *
     C                   EVAL      SVCO# = 0
     C                   EVAL      SVFND = 0
     C                   EVAL      SVEMP# = 0
     C                   EVAL      SVDIV = 0
     C                   EVAL      SVFYR = 0
      *
     C                   ENDSR
      *
     *====================================================================
    *  REFFIRAGT - Refresh First Agent Fields.                          *
     *====================================================================
      *
     C     REFFIRAGT     BEGSR
      *
     C                   EVAL      A2ANM1 = ' '
     C                   EVAL      A2AFE1 = 0
     C                   EVAL      A2AMD1 = 0
     C                   EVAL      A2AAD1 = ' '
     C                   EVAL      A2ACT1 = ' '
     C                   EVAL      A2AST1 = ' '
     C                   EVAL      A2AZP1 = 0
     C                   EVAL      A2ARC1 = 0
     C                   EVAL      A2ARD1 = ' '
     C                   EVAL      A2ARV1 = 0
     C                   EVAL      A2APR1 = 0
     C                   EVAL      A2APN1 = ' '
     C                   EVAL      A2AMT1 = 0
     C                   EVAL      A2NET1 = 0
      *
     C                   ENDSR
      *
     *====================================================================
    *  REFSECAGT - Refresh Second Agent Fields.                         *
     *====================================================================
      *
     C     REFSECAGT     BEGSR
      *
     C                   EVAL      A2ANM2 = ' '
     C                   EVAL      A2AFE2 = 0
     C                   EVAL      A2AMD2 = 0
     C                   EVAL      A2AAD2 = ' '
     C                   EVAL      A2ACT2 = ' '
     C                   EVAL      A2AST2 = ' '
     C                   EVAL      A2AZP2 = 0
     C                   EVAL      A2ARC2 = 0
     C                   EVAL      A2ARD2 = ' '
     C                   EVAL      A2ARV2 = 0
     C                   EVAL      A2APR2 = 0
     C                   EVAL      A2APN2 = ' '
     C                   EVAL      A2AMT2 = 0
     C                   EVAL      A2NET2 = 0
      *
     C                   ENDSR
      *
     *====================================================================
    *  REFCURAGT - Refresh Current Agent Fields.                        *
     *====================================================================
      *
     C     REFCURAGT     BEGSR
      *
     C                   EVAL      A2ANMC = ' '
     C                   EVAL      A2AFEC = 0
     C                   EVAL      A2AMDC = 0
     C                   EVAL      A2AADC = ' '
     C                   EVAL      A2ACTC = ' '
     C                   EVAL      A2ASTC = ' '
     C                   EVAL      A2AZPC = 0
     C                   EVAL      A2ARCC = 0
     C                   EVAL      A2ARDC = ' '
     C                   EVAL      A2ARVC = 0
     C                   EVAL      A2APRC = 0
     C                   EVAL      A2APNC = ' '
     C                   EVAL      A2AMTC = 0
     C                   EVAL      A2NETC = 0
      *
     C                   ENDSR
