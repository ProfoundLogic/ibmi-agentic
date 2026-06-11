     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
    H/TITLE Agent Commission Analysis Report
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DEBUG(*YES)
    H*********************************************************************
    H*  CALLED FROM: W3018C                                              *
    H*                                                                   *
    H*  CALLS TO:  SRCENTER                                              *
    H*                                                                   *
    H*  CREATED: 07/25/08      C:183     PROJECT: 114WQS                 *
    H*********************************************************************
    H*  REVISIONS                                                        *
    H*                                                                   *
     H* 08/26/14 C:307 Project: SY354                                     *
     H*                -Implement Routing Page Changes.                   *
    H*********************************************************************
      *
     FSMCOL     IF   E           K DISK
      *
     FSMORL     IF   E           K DISK
      *
     FWMUPL     IF   E           K DISK
      *
     FWXGSL     IF   E           K DISK
      *
     FWMFNL     IF   E           K DISK
      *
     FWMACL12   IF   E           K DISK
      *
     FWMEML     IF   E           K DISK
      *
     FWMAAL1    IF   E           K DISK
      *
     FWMAGL     IF   E           K DISK
      *
     FWMAHL     IF   E           K DISK
      *
     FWMARL     IF   E           K DISK
      *
     FWMAPL     IF   E           K DISK
      *
     FW3018O    O    E             PRINTER OFLIND(*IN60)
      *
     DXXGRPT           S              9  0
     DXXTOTL           S              9  0
      *
     DXXCRDT           S              8  0
     D@EMEFD1          S               D   DATFMT(*USA)
     D@EMEFDT          S               D   DATFMT(*USA)
     D@ACPODT          S               D   DATFMT(*USA)
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D                 DS
     D  XXBGDT                 1      8  0
     D  XXBGMM                 1      2  0
     D  XXBGDD                 3      4  0
     D  XXBGCC                 5      6  0
     D  XXBGYY                 7      8  0
      *
     D                 DS
     D  XXTRDT                 1      8  0
     D  XXTRMM                 1      2  0
     D  XXTRDD                 3      4  0
     D  XXTRCC                 5      6  0
     D  XXTRYY                 7      8  0
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
      *
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    GSFND
      *
     C     ACKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    GSFND
     C                   KFLD                    XXBGD7
      *
     C     ACKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    GSFND
      *
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ACFND
     C                   KFLD                    ACEMP#
     C                   KFLD                    ACDIV
     C                   KFLD                    ACFYR
     C                   KFLD                    XXCRD7
      *
     C     AHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ACFND
     C                   KFLD                    ACEMP#
     C                   KFLD                    ACDIV
     C                   KFLD                    ACFYR
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ACFND
     C                   KFLD                    ACEMP#
     C                   KFLD                    ACDIV
      *
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    GSFND
     C                   KFLD                    @@AGT#            5 0
      *
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
      *
     C     ARKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    GSFND
     C                   KFLD                    AHRT#
     C                   KFLD                    AHEFDT
      *
     C     ARKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    GSFND
     C                   KFLD                    AHRT#
     C*
     C     APKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C                   KFLD                    @@PROD
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     *LIKE         DEFINE    AAFEIN        @@FEIN
     C     *LIKE         DEFINE    AAMOD         @@MOD
     C     *LIKE         DEFINE    APPROD        @@PROD
     C     *LIKE         DEFINE    RPAGNM        @@RPAGNM
     C     *LIKE         DEFINE    RPPNAM        @@RPPNAM
     C     *LIKE         DEFINE    ACRATE        @AHRATE
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVE(P)   QQCO#         XXCO#             3 0
      *
     C                   EVAL      RSCO# = XXCO#
      *
     C     UPKEY         CHAIN     WMUPL
      *
     C                   EXSR      ROUTEPAGE
      *
     C     XXCO#         CHAIN     SMCOL
     C                   IF        %FOUND(SMCOL)
      *
     C                   EVAL      SRDFLD = CONAME
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      60            SRDLEN            3 0
     C*
     C                   MOVEL(P)  SRDFLD        RPCONM
      *
     C                   ENDIF                                                  %FOUND(SMCOL)
     C*
     C                   EVAL      SRDFLD = 'Agent Commission Analysis Report'
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      60            SRDLEN            3 0
     C*
     C                   MOVEL(P)  SRDFLD        RPNAME
     C*
     C                   EVAL      RPPODT = DSDATE
      *
     C                   EVAL      XXBGDT = DSDATE
      *
     C                   EVAL      XXCRDT = DSDATE
      *
     C                   EVAL      XXBGDD = 1
      *
     C                   IF        XXBGMM <> 1
     C                   EVAL      XXBGMM = XXBGMM - 1
     C                   ELSE
     C                   EVAL      XXBGMM = 12
     C                   EVAL      XXBGMM = 1
     C                   EVAL      XXBGYY = XXBGYY - 1
     C                   ENDIF
      *
     C                   EVAL      SRDTI = XXBGDT
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBGD7            7 0
      *
     C                   EVAL      SRDTI = XXCRDT
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCRD7            7 0
      *
     C                   EVAL      XXGRPT = 0
     C                   EVAL      XXTOTL = 0
      *
     C                   READ      WXGSL
      *
     C                   DOW       NOT%EOF(WXGSL)
      *
     C                   IF        GSIEFL = 'I'
      *
     C     FNKY          CHAIN     WMFNL
      *
     C                   IF        %FOUND(WMFNL)
      *
     C                   EXSR      PRTHEADER
      *
     C                   EXSR      PROC
      *
     C                   ENDIF                                                  %FOUND(WMFNL)
      *
     C                   IF        XXGRPT = 0
     C                   WRITE     NOREC
     C                   ELSE
     C                   EVAL      RPGRPT = XXGRPT
     C                   WRITE     GRPTOT
     C                   EVAL      XXGRPT = 0
     C                   ENDIF                                                  XXGRPT = 0
      *
     C                   ENDIF                                                  GSIEFL = 'I'
      *
     C                   READ      WXGSL
      *
     C                   ENDDO
      *
     C                   EVAL      RPTOTL = XXTOTL
     C                   WRITE     REPTOT
      *
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKEY smorl;
       If %found;
          write Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   EVAL      *INLR = *ON
      *
      ***********************************************************************************
      * PRTHEADER - Print the header, subhead and Column headings
      ***********************************************************************************
     C     PRTHEADER     BEGSR
      *
     C                   EVAL      RPFND = FNFND
     C                   EVAL      RPFNNM = FNNAME
      *
     C                   WRITE     HEADER
      *
     C                   WRITE     SUBHEADER
      *
     C                   WRITE     COLHEAD
      *
     C                   ENDSR
      *
      ***********************************************************************************
      * PROC - Process Agent Commission Records
      ***********************************************************************************
     C     PROC          BEGSR
      *
     C                   Z-ADD     1             XX                2 0
      *
     C     ACKY          SETLL     WMACL12
     C     ACKY1         READE     WMACL12
      *
     C                   DOW       NOT %EOF(WMACL12)
      *
     C                   EVAL      SRDTI = ACTRDT
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
      *
     C                   IF        XXTRYY = XXBGYY AND
     C                             XXTRMM = XXBGMM
      *
     C     AHKY          SETGT     WMAHL
     C     AHKY1         READPE    WMAHL
      *
     C                   DOW       NOT %EOF(WMAHL)
      *
     C                   IF        AHSTAT = 'A'
      *
      * Check the New or Renewal Rate based on ACPODT and EMEFD1 comparison
      *
     C     EMKY          CHAIN     WMEML
     C                   IF        %FOUND(WMEML)
      *
     C     ARKY          SETGT     WMARL
     C     ARKY1         READPE    WMARL
      *
     C                   DOW       NOT %EOF(WMARL) AND ARSTAT = 'I'
     C     ARKY1         READPE    WMARL
     C                   ENDDO
      *
     C                   IF        NOT %EOF(WMARL)
      *
      * If EMEFD1 + 1 Yr > ACPODT then use ARNWRT otherwise use ARRNRT
      *
     C                   EVAL      SRDTI = EMEFD1
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         @@EMEFD1          8 0
     C     *USA          MOVE      @@EMEFD1      @EMEFD1
      *
     C                   EVAL      SRDTI = ACPODT
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         @@ACPODT          8 0
     C     *USA          MOVE      @@ACPODT      @ACPODT
      *
     C     @EMEFD1       ADDDUR    XX:*YEARS     @EMEFDT
      *
     C                   IF        @ACPODT < @EMEFDT
     C                   EVAL      @AHRATE = ARNWRT
     C                   ELSE
     C                   EVAL      @AHRATE = ARRNRT
     C                   ENDIF                                                  @ACPODT < @EMEFDT
      *
     C                   ENDIF                                                  NOT %EOF(WMARL)
      *
     C                   ENDIF                                                  %FOUND(WMEML)
      *
     C                   IF        AHAGT# <> ACAGT# OR @AHRATE <> ACRATE OR
     C                             AHPROD <> ACPROD
      *
     C                   EXSR      DETPRINT
      *
     C                   IF        *IN60 = *ON
     C                   EXSR      PRTHEADER
     C                   EVAL      *IN60 = *OFF
     C                   ENDIF                                                  *IN60 = *ON
      *
     C                   ENDIF                                                  AHAGT# <> ACAGT#
      *
     C                   LEAVE
      *
     C                   ENDIF                                                  AHSTAT = 'A'
      *
     C     AHKY1         READPE    WMAHL
      *
     C                   ENDDO                                                  NOT %EOF(WMAHL)
      *
     C                   ENDIF                                                  XXTRYY = XXBGYY AND
      *
     C     ACKY1         READE     WMACL12
      *
     C                   ENDDO
      *
     C                   ENDSR
      *
      ***********************************************************************************
      * DETPRINT - Printing Detail Records
      ***********************************************************************************
      *
     C     DETPRINT      BEGSR
      *
     C                   EVAL      RPEMP# = ACEMP#
     C                   EVAL      RPDIV = ACDIV
     C                   EVAL      RPFYR = ACFYR
      *
      * Get details for WMACP values
      *
     C                   EVAL      RPAGT# = ACAGT#
     C                   EVAL      RPPROD = ACPROD
     C                   EVAL      RPRATE = ACRATE
      *
     C                   EVAL      @@AGT# = ACAGT#
     C                   EVAL      @@PROD = ACPROD
     C                   EXSR      GETDETAILS
     C                   EVAL      RPAGNM = @@RPAGNM
     C                   EVAL      RPPNAM = @@RPPNAM
      *
      * Get details for WMAHP values
      *
     C                   EVAL      R1AGT# = AHAGT#
     C                   EVAL      R1PROD = AHPROD
     C                   EVAL      R1RATE = @AHRATE
      *
     C                   EVAL      @@AGT# = AHAGT#
     C                   EVAL      @@PROD = AHPROD
     C                   EXSR      GETDETAILS
     C                   EVAL      R1AGNM = @@RPAGNM
     C                   EVAL      R1PNAM = @@RPPNAM
      *
     C                   EVAL      XXGRPT = XXGRPT + 1
     C                   EVAL      XXTOTL = XXTOTL + 1
      *
     C                   WRITE     DETAIL
      *
     C                   ENDSR
      *
      ***********************************************************************************
      * GETDETAILS - Get details from WMAAP, WMAGP and WMARP
      ***********************************************************************************
      *
     C     GETDETAILS    BEGSR
      *
     C     AAKY          CHAIN     WMAAL1
     C                   IF        %FOUND(WMAAL1)
      *
     C                   EVAL      @@FEIN = AAFEIN
     C                   EVAL      @@MOD = AAMOD
      *
     C     AGKY          CHAIN     WMAGL
     C                   IF        %FOUND(WMAGL)
     C                   EVAL      @@RPAGNM = AGNAME
     C                   ENDIF                                                  %FOUND(WMAGL)
      *
     C                   ENDIF                                                  %FOUND(WMAAL1)
      *
     C     APKY          CHAIN     WMAPL
     C                   IF        %FOUND(WMAPL)
     C                   EVAL      @@RPPNAM = %trim(APLSN) + ', ' +
     C                             %trim(APFSN)
     C                   ENDIF
      *
     C                   ENDSR
      *
      ***********************************************************************************
      * ROUTEPAGE - Print Routing Page
      ***********************************************************************************
      *
     C     ROUTEPAGE     BEGSR
      *
     C                   EVAL      XXPRT = '0'
     C     ORKEY         CHAIN     SMORL
      *
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         WRITE     ROUTE
      ***** End Del ***** 08/26/14 ************************************
      *
     C                   ENDSR
