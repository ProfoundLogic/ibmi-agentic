     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H***************************************************************
     H dftactgrp(*no) actgrp(*caller)
     H/TITLE WA076R - EDI Control API.
     H***************************************************************
     H*  Revisions:                                                 *
     H*                                                             *
     H*  07/14/21  C:079  Project: WC2816                           *
     H*                   Add new parm IXUSLH                       *
     H*                                                             *
     H*  06/18/20  C:180  Project: WC2738                           *
     H*                   Add new parm IXDNYDLW                     *
     H*                   Claim denied - Base on day last worked.   *
     H*                                                             *
     H*  03/14/19  C:212  Project: WC2658                           *
     H*                   Add new parms for date overrides          *
     H*                                                             *
     H*  05/30/18  C:274  Project: WC2515                           *
     H*                   Add code for EDI 3.1                      *
     H*                                                             *
     H*  01/15/14  C:53   Project: 147WCJ
     H*                   Add field for EDI start date override
     H*                                                             *
     H*  06/25/12  C:280  SUPPORT 80061                             *
     H*                   Check for inactive status                 *
     H*                                                             *
     H*  04/05/12  C:255  Project WC2087                            *
     H*                   Added Medical Only flag & Send # of days  *
     H*                   Medical.                                  *
     H*                                                             *
     H*  10/26/11  C:180  SUPPORT 78315                             *
     H*                   Check for active status                   *
     H*                                                             *
     H*  05/14/10  C:220  PROJECT: 139WNN                           *
     H*                   EDI extract and FTP automation            *
     H*                   - add new parm                            *
     H*                                                             *
     H*  03/24/10  C:232  PROJECT: 139WNN                           *
     H*                   Created.                                  *
     H*                                                             *
     H***************************************************************
     FWMFNL     IF   E           K DISK
     FWMIXL     IF   E           K DISK
     FWDF3L     IF   E           K DISK
     F
     C     *ENTRY        PLIST
     C                   PARM                    @CO#              3 0
     C                   PARM                    @JRST             2
     C                   PARM                    @TRID             3
     C                   PARM                    @FND              3 0
     C                   PARM                    @RTNCD            1
     C                   PARM                    @EDIF             3
     C                   PARM                    @IXELT            1
     C                   PARM                    @IXELTD           3 0
     C                   PARM                    @IXFROI           2
     C                   PARM                    @IXSDUE           3 0
     C                   PARM                    @IXRLS            1
     C                   PARM                    @IXCSDT           7 0
     C                   PARM                    @IXEADC           5
     C                   PARM                    @IXHCPR           5
     C                   PARM                    @IXFNRC           5
     C                   PARM                    @IXHCED           1
     C                   PARM                    @IXCRER           5
     C                   PARM                    @IXCEIR           5
     C                   PARM                    @IXCECR           5
     C                   PARM                    @IXCTAR           5
     C                   PARM                    @IXCTRR           5
     C                   PARM                    @IXmo             1
     C                   PARM                    @IXmltd           3 0
     C                   PARM                    @IXmsdt           7 0
     C                   PARM                    @IXrsdt           7 0
     C                   PARM                    @IXedny           1
     C                   PARM                    @IXdnyDlw         1
     C*** Beg Add *** 07/14/21 ***********************************************
     C                   Parm                    @IXusLH           1
     C*** End Add *** 07/14/21 ***********************************************
     C
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    @CO#
     C                   KFLD                    @FND
     C
     C*  EDI CLAIMS CONTROL FILE
     C*
     C     IXKY          KLIST
     C                   KFLD                    @CO#
     C                   KFLD                    @JRST
     C                   KFLD                    @TRID
     C                   KFLD                    @EDIF
      *
     C*  Group EDI info
     C*
     C     keyWDF3L      Klist
     C                   Kfld                    @CO#
     C                   Kfld                    @FND
     C                   Kfld                    @jrst2            2
      *
      *
     C                   IF        @EDIF = *blanks
     C*
     C     FNKY          CHAIN     PFN01
     C*
     C                   IF        %FOUND(WMFNL)
     C                   MOVEL     FNEDIF        @EDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
      *
     C     IXKY          CHAIN     PIX01
     C                   IF        %FOUND(WMIXL) AND IXSTAT <> 'I'
      *
      * Processing blank (master for state)
      * Return "Y" if a record exists for the chain to WMIXP. Return
      * "N" if no hit. Blank if not requested in the parms.
      *
     C                   SELECT
      *
     C                   WHEN      @TRID = *BLANKS
     C                   MOVEL     'Y'           @RTNCD
      *
      *  Processing 148 (for state)
      * Return "Y" if a record exists for the chain to WMIXP. Return
      * "N" if no hit. Blank if not requested in the parms.
      *
     C                   WHEN      @TRID  = '148'
     C                   MOVEL     'Y'           @RTNCD
      *
      *  Processing A49 (for state)
      * Return "Y" if a record exists for the chain to WMIXP. Return
      * "N" if no hit. Blank if not requested in the parms.
      *
     C                   WHEN      @TRID  = 'A49'
     C                   MOVEL     'Y'           @RTNCD
      *
      *  Processing RAT (for state)
      * Return "Y" if a record exists for the chain to WMIXP. Return
      * "N" if no hit. Blank if not requested in the parms.
      *
     C                   WHEN      @TRID  = 'RAT'
     C                   MOVEL     'Y'           @RTNCD
      *
     C                   OTHER
      *
     C                   MOVEL     'N'           @RTNCD
      *
     C                   ENDSL
      *
     C                   MOVEL     IXELT         @IXELT
     C                   MOVEL     IXELTD        @IXELTD
     C                   MOVEL     IXFROI        @IXFROI
     C                   MOVEL     IXSDUE        @IXSDUE
     C                   MOVEL     IXRLS         @IXRLS
     C                   MOVEL     IXCSDT        @IXCSDT
     C                   MOVEL     IXEADC        @IXEADC
     C                   MOVEL     IXHCPR        @IXHCPR
     C                   MOVEL     IXFNRC        @IXFNRC
     C                   MOVEL     IXHCED        @IXHCED
     C                   IF        %PARMS > 16
     C                   MOVEL     IXCRER        @IXCRER
     C                   MOVEL     IXCEIR        @IXCEIR
     C                   MOVEL     IXCECR        @IXCECR
     C                   MOVEL     IXCTAR        @IXCTAR
     C                   MOVEL     IXCTRR        @IXCTRR
     C                   ENDIF
      /Free
       If %Parms > 21;
          @IXmo = IXmo;
          @IXmltd = IXmltd;
       Endif;
      /End-Free
      /Free
       If %Parms > 23;
          @IXmsdt = IXmsdt;
          @IXrsdt = IXrsdt;
       Endif;
      /End-Free
       If %Parms > 25;
          @IXedny   = IXedny;
          @IXdnyDlw = IXdnyDlw;
       Endif;
       //*** Beg Add *** 07/14/21 *********************************************************
       If %Parms > 27;
          @IXusLh = IXusLh;
       Endif;
       //*** End Add *** 07/14/21 *********************************************************
      *   EDI 3.1 --> IXRLS = '3' and IXRLSS = '1'
      /Free
       If IXRLS = '3' and IXRLSS = '1';
          @IXRLS  = 'A';
       Endif;
      /End-Free
      /Free
        // Check for Group override date
       @jrst2 = @jrst;
       Chain keyWDF3L pf301;
       If Not %Found(WDF3L);
          @jrst2 = *blanks;
          Chain keyWDF3L pf301;
       EndIf;
       If %Found(WDF3L) and F3ovdt <> 0;
          @IXCSDT = F3ovdt;
       EndIf;
      /End-Free
      *
      *
     C                   ELSE
      *
     C                   MOVEL     *BLANKS       @RTNCD
      *
     C                   ENDIF
      *
     C                   RETURN
