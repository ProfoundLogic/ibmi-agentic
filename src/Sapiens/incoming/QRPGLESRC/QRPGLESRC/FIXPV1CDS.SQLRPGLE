     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H optimize(*none)
     H*****************************************************************
     H/TITLE FIXPV1CDS  - Fix PV1 Codes
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* This program read through FXPV1P and update PV1 codes in      *
     H* FAU1P, WDPAP and WARPPM                                       *
     H*                                                               *
     H*  Created By:                                                  *
     H*  06/24/24  C:079 Ticket: 152/115186                           *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     Ffxpv1p    if   e             Disk
     Ffau1l17   uf   e           k Disk
     Fwdpalg    uf   e           k Disk
     Fwarppml   uf   e           k Disk
     F
     D*================================================================
     D* Stand alone fields
     D*================================================================
     D CurTime         S              6  0
     D CurDate         S              7  0
     D*================================================================
     D* Data Structures
     D*================================================================
     D psds          esds                  extname(szq1p)
       //======================================================================
       // Mainline
       //======================================================================
       Read ppv101;
       Dow Not %Eof(FXPV1P);
          Setll (PV1RVW#1) pu101;
          Reade (PV1RVW#1) pu101;
          Dow Not %Eof(FAU1L17);
             If Pv1Pv1New <> *blanks;
                U1pv1 = Pv1Pv1New;
                Update pu101;
                Exsr UpdatePayFiles;
             Endif;
             Reade (PV1RVW#1) pu101;
          Enddo;
          Read ppv101;
       Enddo;

       *inlr = *on;
       Return;
       //======================================================================
       // Subroutine UpdatePayFiles - Update WDPAP and WARPPM
       //======================================================================
       Begsr UpdatePayFiles;

        CurTime = %Dec(%Char(%Time():*iso0):6:0);
        CurDate = %Dec(%Char(%Date():*cymd0):7:0);

        Setll (U1co#:U1fnd:U1Fyr:U1case:U1eob#) ppa01;
        Reade (U1co#:U1fnd:U1Fyr:U1case:U1eob#) ppa01;
        Dow not %Eof(WDPALG);
           If Pv1Pv1New <> *blanks;
              PAbr2  = Pv1Pv1New;
              PApgm  = Q1pgm;
              PAuser = Q1user;
              PAtime = CurTime;
              PAdt   = CurDate;
              Update ppa01;
           Endif;
           Reade (U1co#:U1fnd:U1Fyr:U1case:U1eob#) ppa01;
        Enddo;

        Setll (U1co#:U1fnd:U1Fyr:U1case:U1eob#) prp01;
        Reade (U1co#:U1fnd:U1Fyr:U1case:U1eob#) prp01;
        Dow not %Eof(WARPPML);
           If Pv1Pv1New <> *blanks;
              RPbr2 = Pv1Pv1New;
              RPpgm = Q1pgm;
              RPuser = Q1user;
              RPdt   = CurDate;
              RPtime = CurTime;
              Update prp01;
           Endif;
           Reade (U1co#:U1fnd:U1Fyr:U1case:U1eob#) prp01;
        Enddo;

       Endsr;
