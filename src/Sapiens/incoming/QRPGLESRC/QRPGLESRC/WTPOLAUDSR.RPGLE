     hcopyright('(c) 2019 Sapiens International. All Rights Reserved.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Toolbar action Audit With SR payrolls.
     h*               - Audits policy using SR payrolls.
     h*
     h*  Called from: Policy toolbar.
     h*  Calls to:
     h*
     h*  Created on 12/16/2019 by C:180 for Project 150WHE
     h**********************************************************************
     h* Revisions:
     h**********************************************************************
     FWDELP     IF   E             DISK    rename(PEL01:PEL0P)
     FWDELL     UF   E           K DISK    prefix(u)
     FWMEPL1    UF   E           K DISK
     FWASRL12   IF   E           K DISK
      *
      * *Entry
      *
     D WTPOLAUDSR      PI
     D inRRN                          9  0
     D svDiv           S                   Like(ELdiv)
     D isFirst         S               N   inz(*on)

     D PSDS          ESDS                  extname(SZQ1P)

      /Free
       svDiv = *Zeros;
       isFirst = *On;
       chain inrrn PEL0P;
       If %found(WDELP);
          Setll (ELco#:ELfnd:ELfyr:ELemp#) PEP01;
          If %equal(WMEPL1);
             Reade (ELco#:ELfnd:ELfyr:ELemp#) PEP01;
             Dow not %Eof(WMEPL1);
                If isFirst;
                   svDiv = EPdiv;
                   isFirst = *off;
                Endif;

                If svDiv <> EPdiv;
                   UpdPolDtl();
                   svDiv = EPdiv;
                Endif;

                EPaapr = GetAuditAmt();
                EPsafl = 'A';
                EPuser = Q1user;
                EPpgm  = Q1pgm;
                EPtime = %int(%char(%time():*ISO0));
                EPdt   = %dec(%date():*cymd);
                Update PEP01;
                Reade (ELco#:ELfnd:ELfyr:ELemp#) PEP01;
             Enddo;
             UpdPolDtl();
          Endif;

       Endif;
       *Inlr = *On;
      /End-Free
     P*-------------------------------------------------------------------
     P* Get Audit Amount
     P*-------------------------------------------------------------------
     P GetAuditAmt     B
     D GetAuditAmt     PI            11  2
     D intTotAudAmt    S             11  2 inz
      /Free
       intTotAudAmt = *Zeros;

       Setll (EPco#:EPfnd:EPfyr:EPemp#:EPdiv:EPclas:EPspec:EPefdt:EPext)
              PSR01;

       If %equal(WASRL12);
          Reade (EPco#:EPfnd:EPfyr:EPemp#:EPdiv:EPclas:EPspec:EPefdt:EPext)
                 PSR01;

          Dow not %eof(WASRL12);
             intTotAudAmt = intTotAudAmt + SRamtp;
             Reade (EPco#:EPfnd:EPfyr:EPemp#:EPdiv:EPclas:EPspec:EPefdt:EPext)
                    PSR01;
          Enddo;
       Endif;

       Return intTotAudAmt;
      /End-Free
     P GetAuditAmt     E
     P*-------------------------------------------------------------------
     P* Update Policy Detail
     P*-------------------------------------------------------------------
     P UpdPolDtl       B
     D UpdPolDtl       PI
      /Free
        Chain (ELco#:ELfnd:ELemp#:svDiv:ELfyr) PEL01;
        uELaudsr = 'Y';
        uELuser = Q1user;
        uELpgm  = Q1pgm;
        uELtime = %int(%char(%time():*ISO0));
        uELdt   = %dec(%date():*cymd) ;
        Update PEL01;
      /End-Free
     P UpdPolDtl       E
      /End-Free
