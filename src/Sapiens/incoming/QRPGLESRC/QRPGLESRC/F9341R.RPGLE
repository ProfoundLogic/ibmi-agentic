     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
    H/TITLE GIM Savings Report - Print Program
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DEBUG(*YES)
     H DFTACTGRP(*NO) ACTGRP(*caller)
    H*********************************************************************
    H*  CALLED FROM: F9341C                                              *
    H*                                                                   *
    H*  CALLS      :                                                     *
    H*                                                                   *
    H*  CREATED: 07/04/07      C:183     PROJECT: 135MJT                 *
    H*********************************************************************
    H*  REVISIONS                                                        *
    H*                                                                   *
     H* 08/26/14 C:307 Project: SY354                                     *
     H*                -Implement Routing Page Changes.                   *
    H*********************************************************************
      *
    H*  REVISIONS                                                        *
     FFAEAL41   IF   E           K DISK
      *
     FFAEAL1    IF   E           K DISK    RENAME(PEA01:PEA02)
     F                                     PREFIX(X_)
      *
     FFAEOL4    IF   E           K DISK
      *
     FFDDAL     IF   E           K DISK
      *
     FWDPALG    IF   E           K DISK
      *
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
      ** GROUP MASTER FILE
     FWMFNL     IF   E           K DISK
      *
     FF9341P    O    E             PRINTER OFLIND(*IN61)
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D@@CO#            S                   LIKE(EACO#)
     D@@SUB#           S                   LIKE(EASUB#)
     D@@FND            S                   LIKE(EAFND)
     D@@FYR            S                   LIKE(EAFYR)
     D@@CASE           S                   LIKE(EACASE)
     D@@EOB#           S                   LIKE(EAEOB#)
     D@@RCN            S                   LIKE(EARCN)
     D@@PCT            S              5  2
     D@@BAMT           S                   LIKE(EABAMT)
     D@@NAMT           S                   LIKE(EANAMT)
     D@@CALL           S              1
     D@@RTCD           S              1
     DTOTBAMTP         S                   LIKE(EABAMT)
     DTOTNAMTP         S                   LIKE(EANAMT)
     DTOTPAIDP         S                   LIKE(EAPAID)
     DTOTPAIDN         S                   LIKE(EAPAID)
     DTOTSAVEP         S                   LIKE(EAMRA)
     DTOTSAVEN         S                   LIKE(EAMRA)
     DHDBAMT           S                   LIKE(EABAMT)
     DKKRCN            S                   LIKE(EARCN)
     DX1FDT            S                   LIKE(PAIDT)
     DX1TDT            S                   LIKE(PAIDT)
      *
     D@XXCLAIM         S             13
     DQQOUTQ           S             10
     DQQMODE           S              1
     DSVFND            S                   LIKE(EAFND)
     DSVCASE           S                   LIKE(EACASE)
     DSVFYR            S                   LIKE(EAFYR)
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     C     *ENTRY        PLIST
     C                   PARM                    @CO#              3
     C                   PARM                    @FND              3
     C                   PARM                    @FDT              8
     C                   PARM                    @TDT              8
      *
     C     FAPLIST       PLIST
     C                   PARM                    @@@RTCD           1
     C                   PARM                    @@@CO#            3
     C                   PARM      *BLANKS       @@@WCF#           3
     C                   PARM                    @@@SUB#           5
     C                   PARM                    @@@FND            3
      *
     C     FNKY          KLIST
     C                   KFLD                    CO#
     C                   KFLD                    EAFND
      *
     C     EOKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
      *
     C     DAKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    EARCN
      *
     C     PAKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    @WCF#
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    @EARCN            7 0
      *
     C     DTYYMD1       PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDY'        SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     EAKY1         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C                   KFLD                    @@EOB#
     C*
     C     EAKY2         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C                   KFLD                    @@EOB#
     C                   KFLD                    @@RCN
      *
     C     DAKY1         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C                   KFLD                    @@EOB#
     C                   KFLD                    KKRCN
      *
     C     COKEY         KLIST
     C                   KFLD                    XXCO#             3 0
      *
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
      *
      *   KEY LIST FOR THE USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
     C                   MOVE      @FDT          SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         @DATCHK           8 0
      *
     C     SRERR         CABEQ     *ON           END                      20
      *
     C                   MOVE      @CO#          CO#               3 0
     C                   EVAL      XXCO# = CO#
     C                   MOVE      @FDT          @@DATE            8 0
      *
      * Pass the From Date in To Date if To Date is Zero
      *
     C                   IF        @TDT = '00000000'
     C                   MOVE      @FDT          @@DATE1           8 0
     C                   ELSE
     C                   MOVE      @TDT          @@DATE1           8 0
     C                   ENDIF
      *
     C                   EXSR      ROUTING
      *
     C                   READ      FAEAL41                                44
      *
     C                   IF        *IN44 = *OFF
     C                   EXSR      PAGEHEADSR
     C                   EXSR      SUBHEADSR
     C                   EXSR      HEADSR
     C                   EVAL      SVFND = EAFND
     C                   EVAL      SVCASE = EACASE
     C                   ENDIF
      *
     C                   DOW       NOT %EOF(FAEAL41)
      *
     C                   MOVE      'Y'           GRNDTOTPRNT       1
      *
     C                   EVAL      @@@CO# = %EDITC(EACO#:'X')
     C                   EVAL      @@@SUB# = %EDITC(EASUB#:'X')
     C                   EVAL      @@@FND = %EDITC(EAFND:'X')
     C                   EVAL      @EARCN = EARCN
      *
     C                   CALL      'FA700R'      FAPLIST
     C                   MOVE      @@@WCF#       @WCF#             3 0
      *
     C                   EXSR      OVRFLWCHK
      *
     C                   SELECT
      *
     C                   WHEN      SVFND <> EAFND
     C                   EXSR      CLMTOTALSR
     C                   EXSR      OVRFLWCHK
     C                   EXSR      GRPTOTALSR
     C                   EXSR      OVRFLWCHK
     C                   EXSR      REFCLMFLD
     C                   EXSR      REFGRPFLD
     C                   EXSR      SUBHEADSR
     C                   EXSR      OVRFLWCHK
     C                   EXSR      HEADSR
     C                   EXSR      OVRFLWCHK
      *
     C                   WHEN      SVCASE <> EACASE
     C                   EXSR      CLMTOTALSR
     C                   EXSR      OVRFLWCHK
     C                   EXSR      REFCLMFLD
     C                   EXSR      SUBHEADSR
     C                   EXSR      OVRFLWCHK
     C                   EXSR      HEADSR
     C                   EXSR      OVRFLWCHK
      *
     C                   ENDSL
      *
     C                   IF        @OVRFLW = 'Y'
     C                   EVAL      @OVRFLW = 'N'
     C                   EXSR      HEADSR
     C                   ENDIF
      *
     C                   EXSR      DETAILSR
      *
     C                   EVAL      SVFND = EAFND
     C                   EVAL      SVCASE = EACASE
      *
     C                   READ      FAEAL41
      *
     C                   ENDDO
      *
     C                   IF        GRNDTOTPRNT = 'Y'
     C                   EXSR      CLMTOTALSR
     C                   EXSR      GRPTOTALSR
     C                   EXSR      GRNTOTALSR
     C                   ENDIF
      *
     C     END           TAG
      *
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
          Write Route;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   EVAL      *INLR = *ON
      *
      *********************************************************************************************
      * OVRFLWCHK - Check Overflow
      *********************************************************************************************
      *
     C     OVRFLWCHK     BEGSR
      *
      *
     C                   IF        *IN61 = *ON
     C                   MOVE      'Y'           @OVRFLW           1
     C                   EVAL      *IN61 = *OFF
     C                   EXSR      PAGEHEADSR
     C                   ENDIF
      *
     C                   ENDSR
      *
      *
      *********************************************************************************************
      * PAGEHEADSR - Print PAGEHEAD Record format
      *********************************************************************************************
      *
     C     PAGEHEADSR    BEGSR
      *
     C                   MOVE      @CO#          CO#               3 0
     C                   MOVE      @FND          FND               3 0
     C     CO#           CHAIN     PCO01                              60
      *
      *   Center company name in field
      *
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        XXCONM           40
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD1
     C                   MOVEL     SRDTO         XXDT
     C                   Z-ADD     DSTIME        XXTIME
      *
     C                   MOVE      @FDT          XXFDT
      *
     C                   IF        @TDT = '00000000'
     C                   MOVE      @FDT          XXTDT
     C                   ELSE
     C                   MOVE      @TDT          XXTDT
     C                   ENDIF
      *
     C                   Z-ADD     XXFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         X1FDT
      *
     C                   Z-ADD     XXTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         X1TDT
      *
     C                   WRITE     PAGEHEAD
      *
     C                   ENDSR
      *
      *********************************************************************************************
      * SUBHEADSR - Print SUBHEAD Record format
      *********************************************************************************************
      *
     C     SUBHEADSR     BEGSR
      *
     C                   EVAL      XXFND = EAFND
      *
     C     FNKY          CHAIN     PFN01                              99
      *
     C                   IF        *IN99 = *OFF
     C                   MOVEL(P)  FNNAME        XXFNAME
     C                   ELSE
     C                   EVAL      XXFND = *ZEROS
     C                   CLEAR                   XXFNAME
     C                   ENDIF
      *
     C     EOKY          CHAIN     PEO01
      *
     C                   IF        %FOUND(FAEOL4)
     C                   EVAL      XXNAME = %TRIM(EOLSN) + ' ' + %TRIM(EOFSN)
     C                             + ' ' + %TRIM(EOMID)
      *
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         @FRDT             8 0
     C                   EVAL      XXFRDT = @FRDT
      *
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         @TODT             8 0
     C                   EVAL      XXTODT = @TODT
      *
     C                   ENDIF
      *
     C                   EVAL      @XXCLAIM = %EDITC(EAFND:'X') +
     C                             %EDITC(EAFYR:'X') + %EDITC(EACASE:'X')
     C                   MOVE      @XXCLAIM      XXCLAIM
      *
     C                   WRITE     SUBHEAD
      *
     C                   ENDSR
      *
      *********************************************************************************************
      * HEADSR - Print HEAD (Column Heading) Record format
      *********************************************************************************************
      *
     C     HEADSR        BEGSR
      *
     C                   WRITE     HEAD
      *
     C                   ENDSR
      *
      *********************************************************************************************
      * DETAILSR - Print DETAIL Record format
      *********************************************************************************************
      *
     C     DETAILSR      BEGSR
      *
     C                   EVAL      XXEOB# = EAEOB#
     C                   EVAL      XXRCN = EARCN
      *
     C                   Z-ADD     EAACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         @ACDT             8 0
     C                   EVAL      XXACDT = @ACDT
      *
     C                   EVAL      XXEPDT = EAEPDT
      *
     C                   Z-ADD     0             @NNDL             6 0
     C                   Z-ADD     0             @CHRG            11 2
     C                   Z-ADD     0             @MRA             11 2
     C                   Z-ADD     0             @OTHR            11 2
     C                   Z-ADD     0             @NDIS            11 2
     C                   Z-ADD     0             @PAID            11 2
     C                   Z-ADD     0             @PFEE            11 2
     C                   Z-ADD     0             @BFEE            11 2
     C                   Z-ADD     0             @TFEE            11 2
     C                   Z-ADD     0             @PAYM            11 2
     C                   Z-ADD     0             @ADJU            11 2
     C                   Z-ADD     0             @TDUE            11 2
      *
     C                   MOVE      'N'           @BILLBASED        1
      *
     C                   IF        EAIDT < 1070417
     C                   MOVE      'Y'           @BILLBASED
     C                   ELSEIF    EAIDT = 1070417 AND EAITIM < 132300
     C                   MOVE      'Y'           @BILLBASED
     C                   ENDIF
      *
     C                   EXSR      CALCFEE
      *
      * Count the total no of lines for the bill
      *
     C     DAKY          CHAIN     FDDAL                              40
      *
     C                   DOW       *IN40 = *OFF
      *
     C                   EXSR      DOND
      *
     C                   IF        (@BILLBASED = 'N' AND @DENIED = 'N')
     C                             OR @BILLBASED = 'Y'
      *
     C                   EVAL      @NNDL = @NNDL + 1
      *
     C                   ENDIF
      *
     C                   IF        (@BILLBASED = 'N' AND  DAPAID <> 0)
     C                             OR @BILLBASED = 'Y'
     C                   EVAL(H)   @CHRG = @CHRG + DACHRG
     C                   EVAL(H)   @MRA  = @MRA  + DAMRA
     C                   EVAL(H)   @OTHR = @OTHR + DAOTHR + DAOTH2
     C                   EVAL(H)   @NDIS = @NDIS + DANDIS
     C                   EVAL(H)   @PAID = @PAID + DAPAID
      *
     C                   ENDIF
      *
     C     DAKY          READE     FDDAL                                  40
      *
     C                   ENDDO
      *
     C     PAKY          CHAIN     WDPALG                             41
      *
     C                   DOW       *IN41 = *OFF
      *
     C                   Z-ADD     PAIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         @PAIDT            8 0
      *
     C                   SELECT
      *
     C****               WHEN      @PAIDT > XXFDT AND @PAIDT < XXTDT
     C                   WHEN      @PAIDT < X1FDT AND @PAIDT > X1TDT
      *
     C                   WHEN      PATYPE = 'P' AND PAFEIN = 751156402
      *
     C                   EVAL      @PAYM = @PAYM + PAAMT
      *
     C                   WHEN      PATYPE = 'A' AND PAFEIN = 751156402
      *
     C                   EVAL      @ADJU = @ADJU + PAAMT
      *
     C                   ENDSL
      *
     C     PAKY          READE     WDPALG                                 41
      *
     C                   ENDDO
      *
     C                   EVAL(H)   @PFEE = @@NAMT
     C                   IF        EONTWK = 'PPC'
     C                   EVAL(H)   @PFEE = EANAMT
     C                   ELSE
     C                   EVAL(H)   @PFEE = 0
     C                   ENDIF
     C                   EVAL(H)   @BFEE = @@BAMT
     C                   EVAL(H)   @BFEE = EABAMT
     C                   EVAL(H)   @TFEE = @PFEE + @BFEE
     C                   EVAL(H)   @TDUE = @TFEE - (@PAYM + @ADJU)
      *
     C                   EVAL      XXNNDL = @NNDL
     C                   EVAL      XXCHRG = @CHRG
     C                   EVAL      XXMRA  = @MRA
     C                   EVAL      XXOTHR = @OTHR
     C                   EVAL      XXNDIS = @NDIS
     C                   EVAL      XXPAID = @PAID
     C                   EVAL      XXPFEE = @PFEE
     C                   EVAL      XXBFEE = @BFEE
     C                   EVAL      XXTFEE = @TFEE
     C                   EVAL      XXPAYM = @PAYM
     C                   EVAL      XXADJU = @ADJU
     C                   EVAL      XXTDUE = @TDUE
      *
     C                   WRITE     DETAIL
      *
      * ACCUMULATE THE CLAIM TOTALS
      *
     C                   EVAL      @CNNDL = @CNNDL + @NNDL
     C                   EVAL      @CCHRG = @CCHRG + @CHRG
     C                   EVAL      @CMRA  = @CMRA  + @MRA
     C                   EVAL      @COTHR = @COTHR + @OTHR
     C                   EVAL      @CNDIS = @CNDIS + @NDIS
     C                   EVAL      @CPAID = @CPAID + @PAID
     C                   EVAL      @CPFEE = @CPFEE + @PFEE
     C                   EVAL      @CBFEE = @CBFEE + @BFEE
     C                   EVAL      @CTFEE = @CTFEE + @TFEE
     C                   EVAL      @CPAYM = @CPAYM + @PAYM
     C                   EVAL      @CADJU = @CADJU + @ADJU
     C                   EVAL      @CTDUE = @CTDUE + @TDUE
      *
      * ACCUMULATE THE GRP TOTALS
      *
     C                   EVAL      @GNNDL = @GNNDL + @NNDL
     C                   EVAL      @GCHRG = @GCHRG + @CHRG
     C                   EVAL      @GMRA  = @GMRA  + @MRA
     C                   EVAL      @GOTHR = @GOTHR + @OTHR
     C                   EVAL      @GNDIS = @GNDIS + @NDIS
     C                   EVAL      @GPAID = @GPAID + @PAID
     C                   EVAL      @GPFEE = @GPFEE + @PFEE
     C                   EVAL      @GBFEE = @GBFEE + @BFEE
     C                   EVAL      @GTFEE = @GTFEE + @TFEE
     C                   EVAL      @GPAYM = @GPAYM + @PAYM
     C                   EVAL      @GADJU = @GADJU + @ADJU
     C                   EVAL      @GTDUE = @GTDUE + @TDUE
      *
      * ACCUMULATE THE GRAND TOTAL
      *
     C                   EVAL      @TNNDL = @TNNDL + @NNDL
     C                   EVAL      @TCHRG = @TCHRG + @CHRG
     C                   EVAL      @TMRA  = @TMRA  + @MRA
     C                   EVAL      @TOTHR = @TOTHR + @OTHR
     C                   EVAL      @TNDIS = @TNDIS + @NDIS
     C                   EVAL      @TPAID = @TPAID + @PAID
     C                   EVAL      @TPFEE = @TPFEE + @PFEE
     C                   EVAL      @TBFEE = @TBFEE + @BFEE
     C                   EVAL      @TTFEE = @TTFEE + @TFEE
     C                   EVAL      @TPAYM = @TPAYM + @PAYM
     C                   EVAL      @TADJU = @TADJU + @ADJU
     C                   EVAL      @TTDUE = @TTDUE + @TDUE
      *
     C                   ENDSR
      *
      *********************************************************************************************
      * CLMSTOTALR - Print Claims Total
      *********************************************************************************************
      *
     C     CLMTOTALSR    BEGSR
      *
     C                   EVAL      XXCNNDL = @CNNDL
     C                   EVAL      XXCCHRG = @CCHRG
     C                   EVAL      XXCMRA  = @CMRA
     C                   EVAL      XXCOTHR = @COTHR
     C                   EVAL      XXCNDIS = @CNDIS
     C                   EVAL      XXCPAID = @CPAID
     C                   EVAL      XXCPFEE = @CPFEE
     C                   EVAL      XXCBFEE = @CBFEE
     C                   EVAL      XXCTFEE = @CTFEE
     C                   EVAL      XXCPAYM = @CPAYM
     C                   EVAL      XXCADJU = @CADJU
     C                   EVAL      XXCTDUE = @CTDUE
      *
     C                   WRITE     CLMTOTAL
      *
     C                   ENDSR
      *
      *********************************************************************************************
      * GRPTOTALSR - Print Group Total
      *********************************************************************************************
      *
     C     GRPTOTALSR    BEGSR
      *
     C                   EVAL      XXGNNDL = @GNNDL
     C                   EVAL      XXGCHRG = @GCHRG
     C                   EVAL      XXGMRA  = @GMRA
     C                   EVAL      XXGOTHR = @GOTHR
     C                   EVAL      XXGNDIS = @GNDIS
     C                   EVAL      XXGPAID = @GPAID
     C                   EVAL      XXGPFEE = @GPFEE
     C                   EVAL      XXGBFEE = @GBFEE
     C                   EVAL      XXGTFEE = @GTFEE
     C                   EVAL      XXGPAYM = @GPAYM
     C                   EVAL      XXGADJU = @GADJU
     C                   EVAL      XXGTDUE = @GTDUE
      *
     C                   WRITE     GRPTOTAL
      *
     C                   ENDSR
      *
      *********************************************************************************************
      * GRNTOTALSR - Print Grand Total
      *********************************************************************************************
      *
     C     GRNTOTALSR    BEGSR
      *
     C                   EVAL      XXTNNDL = @TNNDL
     C                   EVAL      XXTCHRG = @TCHRG
     C                   EVAL      XXTMRA  = @TMRA
     C                   EVAL      XXTOTHR = @TOTHR
     C                   EVAL      XXTNDIS = @TNDIS
     C                   EVAL      XXTPAID = @TPAID
     C                   EVAL      XXTPFEE = @TPFEE
     C                   EVAL      XXTBFEE = @TBFEE
     C                   EVAL      XXTTFEE = @TTFEE
     C                   EVAL      XXTPAYM = @TPAYM
     C                   EVAL      XXTADJU = @TADJU
     C                   EVAL      XXTTDUE = @TTDUE
      *
     C                   WRITE     GRNTOTAL
      *
     C                   ENDSR
      *
      *********************************************************************************************
      * REFCLMFLD - Refresh Claim fields
      *********************************************************************************************
      *
     C     REFCLMFLD     BEGSR
      *
     C                   Z-ADD     0             @CNNDL            6 0
     C                   Z-ADD     0             @CCHRG           11 2
     C                   Z-ADD     0             @CMRA            11 2
     C                   Z-ADD     0             @COTHR           11 2
     C                   Z-ADD     0             @CNDIS           11 2
     C                   Z-ADD     0             @CPAID           11 2
     C                   Z-ADD     0             @CPFEE           11 2
     C                   Z-ADD     0             @CBFEE           11 2
     C                   Z-ADD     0             @CTFEE           11 2
     C                   Z-ADD     0             @CPAYM           11 2
     C                   Z-ADD     0             @CADJU           11 2
     C                   Z-ADD     0             @CTDUE           11 2
      *
     C                   ENDSR
      *
      *********************************************************************************************
      * REFGRPFLD - Refresh Group fields
      *********************************************************************************************
      *
     C     REFGRPFLD     BEGSR
      *
     C                   Z-ADD     0             @GNNDL            6 0
     C                   Z-ADD     0             @GCHRG           11 2
     C                   Z-ADD     0             @GMRA            11 2
     C                   Z-ADD     0             @GOTHR           11 2
     C                   Z-ADD     0             @GNDIS           11 2
     C                   Z-ADD     0             @GPAID           11 2
     C                   Z-ADD     0             @GPFEE           11 2
     C                   Z-ADD     0             @GBFEE           11 2
     C                   Z-ADD     0             @GTFEE           11 2
     C                   Z-ADD     0             @GPAYM           11 2
     C                   Z-ADD     0             @GADJU           11 2
     C                   Z-ADD     0             @GTDUE           11 2
      *
     C                   ENDSR
      *
      *********************************************************************************************
      * REFGRNFLD - Refresh Grand Total fields
      *********************************************************************************************
      *
     C     REFGRNFLD     BEGSR
      *
     C                   Z-ADD     0             @TNNDL            6 0
     C                   Z-ADD     0             @TCHRG           11 2
     C                   Z-ADD     0             @TMRA            11 2
     C                   Z-ADD     0             @TOTHR           11 2
     C                   Z-ADD     0             @TNDIS           11 2
     C                   Z-ADD     0             @TPAID           11 2
     C                   Z-ADD     0             @TPFEE           11 2
     C                   Z-ADD     0             @TBFEE           11 2
     C                   Z-ADD     0             @TTFEE           11 2
     C                   Z-ADD     0             @TPAYM           11 2
     C                   Z-ADD     0             @TADJU           11 2
     C                   Z-ADD     0             @TTDUE           11 2
      *
     C                   ENDSR
      *
      *********************************************************************************************
      * DOND - Denied Or Non-Denied
      *********************************************************************************************
      *
     C     DOND          BEGSR
      *
     C                   MOVE      'Y'           @DENIED           1
      *
     C                   IF        (DARCN# = 0 AND DAPAID <> 0) OR
     C                             (DARCN# > 0 AND (DAPAID <>0 OR
     C                             @@BAMT <> 0 OR @@NAMT <> 0))
      *
     C                   EVAL      @DENIED = 'N'
      *
     C                   ENDIF
      *
     C                   ENDSR
      *
      *********************************************************************************************
      * CALCFEE - CALCULATE FEE
      *********************************************************************************************
      *
     C     CALCFEE       BEGSR
      *
      * LOGIC TO CALCULATE THE BRS AND NTW FEE
      *
     C                   IF        @BILLBASED = 'Y'
     C                   EXSR      BRSFEE1
     C                   ELSE
     C                   EXSR      BRSFEE2
     C                   ENDIF
      *
     C     EOKY          CHAIN     PEO01
     C                   EXSR      PPOFEE
      *
     C*                  CALL      'FA037R'
     C*                  PARM      EACO#         ##CO#
     C*                  PARM      EASUB#        ##SUB#
     C*                  PARM      EAFND         ##FND
     C*                  PARM      EAFYR         ##FYR
     C*                  PARM      EACASE        ##CASE
     C*                  PARM      EAEOB#        ##EOB#
     C*                  PARM      EARCN         ##RCN
     C*                  PARM      0.17          ##PCT
     C*                  PARM      *ZEROS        ##BAMT
     C*                  PARM      'P'           ##CALL
     C*                  PARM      ' '           ##RTCD
     c*
     c*                  CALL      'FA038R'
     C*                  PARM      EACO#         ##CO#
     C*                  PARM      EASUB#        ##SUB#
     C*                  PARM      EAFND         ##FND
     C*                  PARM      EAFYR         ##FYR
     C*                  PARM      EACASE        ##CASE
     C*                  PARM      EAEOB#        ##EOB#
     C*                  PARM      EARCN         ##RCN
     C*                  PARM      0.25          ##PCT
     C*                  PARM      *ZEROS        ##NAMT
     C*                  PARM      'P'           ##CALL
      *
     C                   ENDSR
      *
      *
      *********************************************************************************************
      * BRSFEE1 - CALCULATE BRS FEE FOR EAIDT < 04172007 AND EAITIM < 132300
      *********************************************************************************************
      *
     C     BRSFEE1       BEGSR
      *
     C                   EVAL      @@CO# = EACO#
     C                   EVAL      @@SUB#= EASUB#
     C                   EVAL      @@FND = EAFND
     C                   EVAL      @@FYR = EAFYR
     C                   EVAL      @@CASE= EACASE
     C                   EVAL      @@EOB#= EAEOB#
     C                   EVAL      @@RCN = EARCN
     C                   EVAL      @@PCT = 0.17
     C                   EVAL      @@BAMT= *ZEROS
     C                   EVAL      @@CALL= 'P'
     C                   EVAL      @@RTCD= ' '
      *
     C                   EVAL      TOTBAMTP = 0
     C                   EVAL      TOTPAIDP = 0
     C                   EVAL      TOTPAIDN = 0
     C                   EVAL      TOTSAVEP = 0
     C                   EVAL      TOTSAVEN = 0
     C*
     C                   IF        @@RCN = 0
     C     EAKY2         CHAIN     PEA02
     C                   IF        X_EAPAID = 0
     C                   EVAL      @@BAMT = 0
     C                   ELSE
     C                   EVAL      HDBAMT = 0
     C                   EVAL      KKRCN = X_EARCN
     C     DAKY1         CHAIN     PDA01
     C                   DOW       NOT *IN99
     C                   EVAL      HDBAMT = HDBAMT + DAMRA + DAOTHR + DAOTH2
     C     DAKY1         READE     PDA01                                  99
     C                   ENDDO
     C*
     C                   EVAL      @@BAMT = HDBAMT * @@PCT
     C                   ENDIF
     C                   ELSE
     C     EAKY1         CHAIN     PEA02                              99
     C                   DOW       NOT *IN99
     C                   IF        X_EARCN < @@RCN
     C                   EVAL      TOTBAMTP = TOTBAMTP + X_EABAMT
     C                   EVAL      TOTPAIDP = TOTPAIDP + X_EAPAID
     C                   EVAL      KKRCN = X_EARCN
     C     DAKY1         CHAIN     PDA01
     C                   DOW       NOT *IN99
     C                   EVAL      TOTSAVEP = TOTSAVEP + DAMRA + DAOTHR +
     C                                        DAOTH2
     C     DAKY1         READE     PDA01                                  99
     C                   ENDDO
     C                   ELSE
     C                   IF        X_EARCN = @@RCN
     C                   EVAL      TOTPAIDN = TOTPAIDP + X_EAPAID
     C                   EVAL      KKRCN = X_EARCN
     C     DAKY1         CHAIN     PDA01
     C                   DOW       NOT *IN99
     C                   EVAL      TOTSAVEN = TOTSAVEN + DAMRA + DAOTHR +
     C                                        DAOTH2
     C     DAKY1         READE     PDA01                                  99
     C                   ENDDO
     C*
     C                   EVAL      TOTSAVEN = TOTSAVEN + TOTSAVEP
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C     EAKY1         READE     PEA02                                  99
     C                   ENDDO
     C*
     C                   IF        TOTPAIDP = 0
     C                   IF        TOTPAIDN = 0
     C                   EVAL      @@BAMT = 0
     C                   ELSE
     C                   EVAL      @@BAMT = TOTSAVEN * @@PCT
     C                   EVAL      @@RTCD = 'P'
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   IF        TOTPAIDN = 0
     C                   IF        @@CALL = 'P'
     C                   EVAL      @@BAMT = TOTSAVEN * @@PCT * -1
     C                   ELSE
     C                   EVAL      @@BAMT = TOTBAMTP * -1
     C                   ENDIF
     C                   ELSE
     C     EAKY2         CHAIN     PEA02
     C                   IF        %FOUND
     C                   EVAL      KKRCN = X_EARCN
     C                   EVAL      HDBAMT = 0
     C     DAKY1         CHAIN     PDA01
     C                   DOW       NOT *IN99
     C                   EVAL      HDBAMT = HDBAMT + DAMRA + DAOTHR + DAOTH2
     C     DAKY1         READE     PDA01                                  99
     C                   ENDDO
     C*
     C                   EVAL      @@BAMT = HDBAMT * @@PCT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR
      *
      *
      *********************************************************************************************
      * BRSFEE2 - CALCULATE BRS FEE OTHERWISE
      *********************************************************************************************
      *
     C     BRSFEE2       BEGSR
      *
     C                   EVAL      @@CO# =  EACO#
     C                   EVAL      @@SUB#=  EASUB#
     C                   EVAL      @@FND =  EAFND
     C                   EVAL      @@FYR =  EAFYR
     C                   EVAL      @@CASE=  EACASE
     C                   EVAL      @@EOB#=  EAEOB#
     C                   EVAL      @@RCN =  EARCN
     C                   EVAL      @@PCT =  0.17
     C                   EVAL      @@BAMT=  *ZEROS
     C                   EVAL      @@CALL=  'P'
     C                   EVAL      @@RTCD=  ' '
      *
     C                   EVAL      TOTBAMTP = 0
     C                   EVAL      TOTPAIDP = 0
     C                   EVAL      TOTPAIDN = 0
     C                   EVAL      TOTSAVEP = 0
     C                   EVAL      TOTSAVEN = 0
     C*
     C                   IF        @@RCN = 0
     C     EAKY2         CHAIN     PEA02
     C                   IF        X_EAPAID = 0
     C                   EVAL      @@BAMT = 0
     C                   ELSE
     C                   EVAL      HDBAMT = 0
     C                   EVAL      KKRCN = X_EARCN
     C     DAKY1         CHAIN     PDA01
     C                   DOW       NOT *IN99
     C                   IF        DAPAID <> 0
     C                   EVAL      HDBAMT = HDBAMT + DAMRA + DAOTHR + DAOTH2
     C                   ENDIF
     C     DAKY1         READE     PDA01                                  99
     C                   ENDDO
     C*
     C                   EVAL      @@BAMT = HDBAMT * @@PCT
     C                   ENDIF
     C                   ELSE
     C     EAKY1         CHAIN     PEA02                              99
     C                   DOW       NOT *IN99
     C                   IF        X_EARCN < @@RCN
     C                   EVAL      TOTBAMTP = TOTBAMTP + X_EABAMT
     C                   EVAL      TOTPAIDP = TOTPAIDP + X_EAPAID
     C                   EVAL      KKRCN = X_EARCN
     C     DAKY1         CHAIN     PDA01
     C                   DOW       NOT *IN99
     C                   IF        DAPAID <> 0
     C                   EVAL      TOTSAVEP = TOTSAVEP + DAMRA + DAOTHR +
     C                                        DAOTH2
     C                   ENDIF
     C     DAKY1         READE     PDA01                                  99
     C                   ENDDO
     C                   ELSE
     C                   IF        X_EARCN = @@RCN
     C                   EVAL      TOTPAIDN = TOTPAIDP + X_EAPAID
     C                   EVAL      KKRCN = X_EARCN
     C     DAKY1         CHAIN     PDA01
     C                   DOW       NOT *IN99
     C                   IF        DAPAID <> 0
     C                   EVAL      TOTSAVEN = TOTSAVEN + DAMRA + DAOTHR +
     C                                        DAOTH2
     C                   ENDIF
     C     DAKY1         READE     PDA01                                  99
     C                   ENDDO
     C*
     C                   EVAL      TOTSAVEN = TOTSAVEN + TOTSAVEP
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C     EAKY1         READE     PEA02                                  99
     C                   ENDDO
     C*
     C                   IF        TOTPAIDP = 0
     C                   IF        TOTPAIDN = 0
     C                   EVAL      @@BAMT = 0
     C                   ELSE
     C                   EVAL      @@BAMT = TOTSAVEN * @@PCT
     C                   EVAL      @@RTCD = 'P'
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   IF        TOTPAIDN = 0
     C                   IF        @@CALL = 'P'
     C                   EVAL      @@BAMT = TOTSAVEN * @@PCT * -1
     C                   ELSE
     C                   EVAL      @@BAMT = TOTBAMTP * -1
     C                   ENDIF
     C                   ELSE
     C     EAKY2         CHAIN     PEA02
     C                   IF        %FOUND
     C                   EVAL      KKRCN = X_EARCN
     C                   EVAL      HDBAMT = 0
     C     DAKY1         CHAIN     PDA01
     C                   DOW       NOT *IN99
     C                   IF        DAPAID <> 0
     C                   EVAL      HDBAMT = HDBAMT + DAMRA + DAOTHR + DAOTH2
     C                   ENDIF
     C     DAKY1         READE     PDA01                                  99
     C                   ENDDO
     C*
     C                   EVAL      @@BAMT = HDBAMT * @@PCT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR
      *
      *********************************************************************************************
      * PPOFEE - CALCULATE PPO FEE
      *********************************************************************************************
      *
     C     PPOFEE        BEGSR
      *
     C                   EVAL      @@CO# =  EACO#
     C                   EVAL      @@SUB#=  EASUB#
     C                   EVAL      @@FND =  EAFND
     C                   EVAL      @@FYR =  EAFYR
     C                   EVAL      @@CASE=  EACASE
     C                   EVAL      @@EOB#=  EAEOB#
     C                   EVAL      @@RCN =  EARCN
     C                   EVAL      @@PCT =  0.25
     C                   EVAL      @@NAMT=  *ZEROS
     C                   EVAL      @@CALL=  'P'
     C                   EVAL      @@RTCD=  ' '
      *
     C                   EVAL      TOTNAMTP = 0
     C                   EVAL      TOTPAIDP = 0
     C                   EVAL      TOTPAIDN = 0
     C                   EVAL      TOTSAVEP = 0
     C                   EVAL      TOTSAVEN = 0
      *
     C                   IF        @@RCN = 0
     C     EAKY2         CHAIN     PEA02
     C                   IF        X_EAPAID = 0
     C                   EVAL      @@NAMT = 0
     C                   ELSE
     C                   EVAL      @@NAMT = EANDIS * @@PCT
     C                   ENDIF
     C                   ELSE
     C     EAKY1         CHAIN     PEA02                              99
     C                   DOW       NOT *IN99
     C                   IF        X_EARCN < @@RCN
     C                   EVAL      TOTNAMTP = TOTNAMTP + X_EANAMT
     C                   EVAL      TOTPAIDP = TOTPAIDP + X_EAPAID
     C                   EVAL      TOTSAVEP = TOTSAVEP + X_EANDIS
     C                   ELSE
     C                   IF        X_EARCN = @@RCN
     C                   EVAL      TOTPAIDN = TOTPAIDP + X_EAPAID
     C                   EVAL      TOTSAVEN = TOTSAVEP + X_EANDIS
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C     EAKY1         READE     PEA02                                  99
     C                   ENDDO
     C*
     C                   IF        TOTPAIDP = 0
     C                   IF        TOTPAIDN = 0
     C                   EVAL      @@NAMT = 0
     C                   ELSE
     C                   EVAL      @@NAMT = TOTSAVEN * @@PCT
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   IF        TOTPAIDN = 0
     C                   IF        @@CALL = 'P'
     C                   EVAL      @@NAMT = TOTSAVEN * @@PCT * -1
     C                   IF        @@NAMT = 0 AND TOTNAMTP <> 0
     C                   EVAL      @@NAMT = TOTNAMTP * -1
     C                   ENDIF
     C                   ELSE
     C                   EVAL      @@NAMT = TOTNAMTP * -1
     C                   ENDIF
     C                   ELSE
     C     EAKY2         CHAIN     PEA02
     C                   IF        %FOUND
     C                   EVAL      @@NAMT = X_EANDIS * @@PCT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR
      *
      *********************************************************************************************
      * ROUTING - Print Routing Page
      *********************************************************************************************
      *
     C     ROUTING       BEGSR
      *
     C*   GET THE DISTRIBUTION INFORMATION
      *
     C                   EVAL      QQMODE = 'B'
      *
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   ENDIF                                                  QQMODE IFEQ
      *
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C                   EVAL      QQOUTQ = OROUTQ
      *
     C*   CHAIN TO THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
      *
     C                   EVAL      RSCO# = XXCO#
     C                   EVAL      RSDATE = @@DATE
     C                   EVAL      RSDATE1 = @@DATE1
      *
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         WRITE     ROUTE
      ***** End Del ***** 08/26/14 ************************************
      *
     C                   ENDSR
