     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')
     H/TITLE SR32601B - LOAD NC CANCELLATION AND NON-RENEWAL ENDORSEMENT
      *===============================================================*
      * WC320601B - NC Cancellation and Non-Renewal Endorsement       *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 12/06/19                                          *
      * Programmer: C:079                                             *
      * Project   : WC2723                                            *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
      * 10/19/22 C:342 Project: WC2834                                *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      *===============================================================*
      * Notes:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      *                                                               *
      *  QQMODE = 1 - Called to determine if form should be attached. *
      *  QQMODE = 2 - Called to print endorsement.                    *
      *  QQMODE = 3 - Called for additional input.                    *
      *  QQMODE = 4 - Called for mid-term processing.                 *
      *===============================================================*
      ** Copybook F specs
     F/COPY SCOPYEN01F
     FWTCRL2    IF   E           K DISK
     FWMECL1    IF   E           K DISK
     D SR32601B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR32601B        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
      *
      *=========================================================================
      *  Arrays
      *=========================================================================
     D CC              S             10A   DIM(3000)    inz

      ** Persons, organizations named in the schedule
     D NameDS          DS
     D    FM2001               1    200
     D    FM2002             201    400
     D    FM2003             401    600
     D    FM2004             601    800
     D    FM2005             801   1000
     D    FM2006            1001   1200
     D    FM2007            1201   1400
     D    FM2008            1401   1600
     D    FM2009            1601   1800
     D    FM2010            1801   2000
     D    @Nam                      200    Overlay(NameDS) DIM(10)
      *=========================================================================
      *  /COPY required constants
      *=========================================================================
      ** Form ID
     D QQform          C                   CONST('WC320601B ')
     D NonPay          C                   CONST('05')
     D NonRen          C                   CONST('00')
     D Idx             s              5  0 INZ
     D Nix             s              5  0 INZ
      *
      ** Additional entry program
     D ADDPGM          C                   CONST('SRXXXXXXX ')

     D/COPY SCOPYEN01D


      *  Main
      *=========================================================================
      ** Copybook C specs
     C/COPY SCOPYEN01C
      /Free
       //=========================================================================
       // OTHINF - Subroutine to load form specific information & write record
       //=========================================================================
       Begsr OthInf;

       setll (xxco#:xxfnd:xxfyr:xxemp#) pel02;
       reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;

       Dow not %eof(WDELL1);
          //***** Beg Add *** 10/19/22 ******************************
          xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
          //***** End Add *** 10/19/22 ******************************
          Chain (Elco#:ELrtcd) prs01;
          If RSjrst = ENst;

             //Non-Payment
             FMD601 = *Zeros;
             *in27 = *off;
             Setgt (xxco#:enst:NonPay:elpfdt) WTCRL2;
             Readpe (xxco#:enst:NonPay) WTCRL2;
             Dow not %eof(WTCRL2);
                Nix = %Lookup(CRkey:cc);
                If CRstat = 'A' and CRefdt <= ELpfdt;
                   If *in27 = *on;
                      If CRnotc <= FMD601 and Nix = *Zeros;
                         FMD601 = CRnotc;
                         Idx = Idx + 1;
                         cc(idx) = CRkey;
                      Endif;
                   Else;
                      FMD601 = CRnotc;
                      idx = idx + 1;
                      cc(idx) = CRkey;
                      *In27 = *on;
                   Endif;
                Endif;
                Readpe (xxco#:enst:NonPay) WTCRL2;
             Enddo;

             //Non-Renewal
             FMD602 = *Zeros;
             *in27 = *off;
             Reset cc;
             Nix = *zeros;
             Idx = *zeros;
             Setgt (xxco#:enst:NonRen:elpfdt) WTCRL2;
             Readpe (xxco#:enst:NonRen) WTCRL2;
             Dow not %eof(WTCRL2);
                Nix = %Lookup(CRkey:cc);
                If CRstat = 'A' and CRefdt <= ELpfdt;
                   If *in27 = *on;
                      If CRnotc <= FMD602 and Nix = *Zeros;
                         FMD602 = CRnotc;
                         Idx = Idx + 1;
                         cc(idx) = CRkey;
                      Endif;
                   Else;
                      FMD602 = CRnotc;
                      idx = idx + 1;
                      cc(idx) = CRkey;
                      *In27 = *on;
                   Endif;
                Endif;
                Readpe (xxco#:enst:NonRen) WTCRL2;
             Enddo;


             //Other reasons
             FMD603 = *Zeros;
             *in27 = *off;
             Reset cc;
             Nix = *zeros;
             Idx = *zeros;
             Setll (xxco#:enst) WTCRL2;
             reade (xxco#:enst) WTCRL2;

             Dow not %eof(WTCRL2);
                Nix = %Lookup(CRkey:cc);
                If CRncci <> '05' and CRncci <> '00' and
                   CRstat = 'A' and CRefdt <= ELpfdt;
                   If *In27 = *on;
                      If CRnotc <= FMD603 and Nix = *Zeros;
                         FMD603 = CRnotc;
                         Idx = Idx + 1;
                         cc(idx) = CRkey;
                      Endif;
                   Else;
                      FMD603 = CRnotc;
                      idx = idx + 1;
                      cc(idx) = CRkey;
                      *In27 = *on;
                   Endif;
                Endif;
                reade (xxco#:enst) WTCRL2;
             Enddo;

             //Persons, Organizations in the schedule
             Reset @Nam;
             Idx = *zeros;
             Setll (xxco#:xxfnd:xxemp#:ELdiv) WMECL1;
             Reade (xxco#:xxfnd:xxemp#:ELdiv) WMECL1;
            //
            // For now, do not populate persons/organizations in schedule
            //
            // Dow not %eof(WMECL1);
            //    If ECpcnt = '1' or ECpcnt = '2';
            //       Idx = Idx + 1;
            //       @Nam(Idx) = %Trim(ECfsn) + ' ' + %Trim(ECmid) + ' ' +
            //                   %Trim(EClsn);
            //    Endif;
            //    Reade (xxco#:xxfnd:xxemp#:ELdiv) WMECL1;
            // Enddo;
             Exsr WriteWorkFile;
          Endif;
          //***** Beg Add *** 10/19/22 ******************************
          Endif;
          //***** End Add *** 10/19/22 ******************************
          reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
       Enddo;
       Endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr Valid;
       Chain (xxco#:elrtcd) prs01;
       If %found and rsjrst = enst;
          Setll (xxco#:enst) WTCRL2;
          If %equal;
             crtfrm = *on;
          Endif;
       Endif;
       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr PgmStr;
       WriteInCopybook = *Off;
       Endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr PgmEnd;
       Endsr;
      /End-Free
