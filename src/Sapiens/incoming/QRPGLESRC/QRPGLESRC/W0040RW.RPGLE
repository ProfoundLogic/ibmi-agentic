     H* (c) 2025 Sapiens. All Rights Reserved.
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*****************************************************************
     H/Title W0040RW - Work with Agent Master Information Wrapper
     H*****************************************************************
     H* Created: 10/03/12  C:190   Project: WC2116                    *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 05/27/25 C:291 Support: 117250                                *
     H*                Call STDRVPGM instead of WTAGTPRI for all other*
     H*                action panel options to work.                  *
     H*                                                               *
     H* 09/10/24 C:291 Support: 117250                                *
     H*                Call new redesigned Agency screen from Agency  *
     H*                link in Policy Action Panel.                   *
     H*                                                               *
     H*****************************************************************
     F* Files Used
     F*** Beg Add ***** 09/10/24  *********************************
     Fwmahp     if   e             disk
     Fwmaal1    if   e           k disk    rename(paa01:paa01)
     Fwmagl     if   e           k disk    rename(pag01:pag00) infds(agfds)

     D agfds           ds
     D agrrn                 397    400b 0

     D @@rrn1          s              9
     D @@mode1         s              1
     F*** End Add ***** 09/10/24  *********************************

     D @@kc            s              1
     D @@kl            s              1
     D @@call          s              1
     D @@rrn           s              9
     D xxrrn           s              9  0
     D xxview          s              1  0
     D @@view          s              1
     D @@quote         s              1
     D @@mode          s              1

     D @@Next1         s             10a
     D @@TabInd1       s              5a

     F*** Beg Add ***** 05/27/25  *********************************
     DDriverPgm        pr                  extpgm('STDRVPGM')
     D inRrn                          9
     D inMode                         1
     D inNext                        10
     D inTab                          5
     F*** End Add ***** 05/27/25  *********************************

    D*================================================================
    D*  Procedure Definitations
    D*================================================================
     D W0040R          pr                  extPgm('W0040R')
     D @@kc                                like(@@kc)
     D @@kl                                like(@@kl)
     D @@call                              like(@@call)
     D @@rrn                               like(xxrrn)
     D @@view                              like(xxview)
     D @@quote                             like(@@quote)
     D @@mode                              like(@@mode)

     D WTAGTPRI        pr                  extPgm('WTAGTPRI')
     D @@rrn                          9
     D @@mode                         1a
     D @@Next1                       10a
     D @@TabInd1                      5a
    C*================================================================
    C*  *Entry
    C*================================================================
     C     *entry        plist
     C                   parm                    @@kc
     C                   parm                    @@kl
     C                   parm                    @@call
     C                   parm                    @@rrn
     C                   parm                    @@view
     C                   parm                    @@quote
     C                   parm                    @@mode

      /free

       If @@rrn <> ' ';
          xxRrn = %int(@@rrn);
       Endif;

       If @@view <> ' ';
          xxview = %int(@@view);
       Endif;

       //*** Beg Chg ***** 09/10/24 **************************
       //W0040R(@@kc:@@kl:@@call:xxRrn:xxview:@@quote:@@mode);
       Chain xxRrn pah01;
       Chain (AHco#:AHfnd:AHagt#) paa01;
       Chain (AAco#:AAfein:AAmod) pag00;
       @@Rrn1 = %Editc(AGrrn:'X');
       @@mode1 = 'D';
       //*** Beg Chg ***** 05/27/25  *********************************
       //***WTAGTPRI(@@Rrn1:@@mode1:@@Next1:@@TabInd1);
       @@Next1 = 'WTAGTPRI';
       @@TabInd1 = '00000';
       DriverPgm(@@Rrn1:@@Mode1:@@Next1:@@TabInd1);
       //*** End Chg ***** 05/27/25  *********************************
       //*** End Chg ***** 09/10/24 **************************

       Return;

      /end-free
