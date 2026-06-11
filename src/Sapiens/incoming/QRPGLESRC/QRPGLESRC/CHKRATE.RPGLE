     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
    H*********************************************************************
    H/TITLE CHKRATE - Calculate rate and compare to what is in WMEPP.
    H*********************************************************************
    H*                                                                   *
    H* DESCRIPTION:  Read over employer payroll records from date passed *
    H*               in and forward and compare each payroll rate on     *
    H*               each payroll record compared to what should         *
    H*               calculate based on single rounding post ticket      *
    H*               119563.                                             *
    H*                                                                   *
    H* CALLED FROM:                                                      *
    H*   COMMAND LINE                                                    *
    H* CALLS TO:                                                         *
    H*   NONE                                                            *
    H*                                                                   *
    H* CREATED:  10/20/25     C:65     SUPPORT: Phw#119967               *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
    H*********************************************************************

     Fwmeple    if   e           k disk
     Fwdell     if   e           k disk
     Fwmrsl     if   e           k disk
     Fwmprl1    if   e           k disk
     Fwdhel     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     Dpsds           esds                  extname(szq1p)
     d x2arat          s              7s 2 Inz(*Zero)

     C*
     C     RSKY          KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    ELRTCD
     C*
     c     elky          klist
     c                   kfld                    EPCO#
     c                   kfld                    EPFND
     c                   kfld                    EPEMP#
     c                   kfld                    EPDIV
     c                   kfld                    EPFYR

     c     prky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELRTCD
     c                   kfld                    EPCLAS

     c     prky1         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELRTCD
     c                   kfld                    EPCLAS
     c                   kfld                    X3EFDT
     C*
     C     HEKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELPFDT
     C*
     C     HEKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDate            7
     C*
     C                   Move      QQDate        xxDate            7 0
     C                   MOVE      *ON           *INOF
     C*
     C                   Read      pep01
     C                   Dow       not(%eof)

     c                   If        EPDt >= xxDate and EPSTAT = 'A'

     c     elky          Chain     pel01
     c                   If        %found and ELFAF <> ' ' and (ELSTAT <> 'Y' or
     C                             (ELSTAT = 'Y' and PRARFL <> 'Y'))
     C*
     C     rsky          Chain     prs01                              60
     c                   If        %found
     C*
     C                   Z-ADD     0             X3EFDT            7 0
     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   EPEFDT        ##DATE            7
     C                   MOVE(P)   'P'           ##TYPE            1
     C                   MOVE(P)   EPCLAS        ##CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##DATE
     C                   PARM                    ##TYPE
     C                   PARM                    ##CLAS
     C*
     C                   MOVE      ##DATE        X3EFDT
     C*
     c     prky1         setgt     ppr01
     c     prky          readpe    ppr01
     C*
     c                   If        not(%eof)
     C*
     C                   MOVE(P)   ELCO#         ##CO#
     C                   MOVE(P)   ELFND         ##FND
     C                   MOVE(P)   PRRTCD        ##RTCD
     C                   MOVE(P)   PRCLAS        ##CLAS
     C                   MOVE(P)   X3EFDT        ##EFDT
     C                   CALL      'SRLCM'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##RTCD            3
     C                   PARM                    ##CLAS            5
     C                   PARM                    ##EFDT            7
     C                   PARM      *BLANKS       QQLCM             5
     C*
     C                   Z-add     1             xxlcm
     C*
     C     QQLCM         IFNE      *BLANKS
     C                   MOVE      QQLCM         XXLCM             5 4
     C                   ENDIF
     C*
     C* Round/calculate payroll rate to 2 digits if not approved yet for 3.
     C                   If        RS3Date = 0 or ELPfdt < RS3Date
     C                   Eval(h)   x2arat = xxlcm * prrt
     C                   Z-add     x2arat        PRRT
     C                   Else
     C                   Eval(h)   prrt = xxlcm * prrt
     C                   Endif
     C*
     C                   If        EPSpec = 'F' and PRLHCD = 'N'
     C*
     C     HEKY          SETGT     PHE01
     C     HEKY2         READPE    PHE01                                  60
     C     *IN60         IFEQ      *OFF
     C* Round/calculate payroll rate to 2 digits if not approved yet for 3.
     C                   If        RS3Date = 0 or ELPFDT < RS3Date
     C     PRRT          MULT      HELHNF        x2arat
     C                   Z-add     x2arat        PRRT
     C                   Else
     C     PRRT          MULT      HELHNF        PRRT
     C                   Endif
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  EPSPEC IFEQ
     C*
     C                   If        EPARAT <> PRRT
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C                   EXCEPT    DET
     C*
     c                   Endif

     c                   Endif

     c                   Endif

     c                   Endif

     c                   Endif

     C                   Read      pep01
     C                   Enddo
     C*
     C                   Eval      *inlr = *on
     C********************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                                           65 'CHECK PAYROLL RATE NOT '
     O                                           87 'MATCHING ROUNDED RATE.'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                            5 'CO#'
     O                                           10 'FND'
     O                                           22 'EMPLOYER #'
     O                                           32 'DIVISION'
     O                                           37 'FYR'
     O                                           48 'EFF DATE'
     O                                           55 'CLASS'
     O                                           61 'SPEC'
     O                                           67 'EXT'
     O                                           77 'EPARAT'
     O                                           87 'CALC RATE'
     O                                          100 'MAINT DATE'
     O          E            DET            1
     O                       EPCO#         4      5
     O                       EPFND         4     10
     O                       EPEMP#        4     22
     O                       EPDIV         4     30
     O                       EPFYR         4     37
     O                       EPEFDT              48 '   /  /  '
     O                       EPCLAS              55
     O                       EPSPEC              60
     O                       EPEXT               66
     O                       EPARAT        3     76
     O                       PRRT          3     86
     O                       EPDT                99 '   /  /  '
