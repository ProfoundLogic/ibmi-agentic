     Hcopyright('(c) 2025 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*****************************************************************
     H/Title W0042R02W - Producer Maintenance Wrapper
     H*****************************************************************
     H* Created: 11/06/12  C:062   Project: WC2116                    *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 09/01/25 C:300 Support: 117250                                *
     H*                Call new redesigned Agency Producers screen from
     H*                Producers link in Policy Action Panel.         *
     H*                                                               *
     H*****************************************************************
     F* Files Used

     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9
     D @@rrn2          s              9
     D xxrrn           s              9  0
     D xxrrn2          s              9  0
     F*** Beg Add ***** 09/01/25  *********************************
     D @@rrn1          s              9
     D @@mode1         s              1
     D @@Next1         s             10a
     D @@TabInd1       s              5a
     F*** End Add ***** 09/01/25  *********************************

    D*================================================================
    D*  Procedure Definitations
    D*================================================================
     D W0042R02        pr                  extPgm('W0042R02')
     D exit                                like(@@kc)
     D cancel                              like(@@kl)
     D mode                                like(@@mode)
     D producerRrn                         like(xxrrn)
     D agentRrn                            like(xxrrn2)

     F*** Beg Add ***** 09/01/25  *********************************
     DDriverPgm        pr                  extpgm('STDRVPGM')
     D inRrn                          9
     D inMode                         1
     D inNext                        10
     D inTab                          5
     F*** End Add ***** 09/01/25  *********************************

    C*================================================================
    C*  *Entry
    C*================================================================
     C     *entry        plist
     C                   parm                    @@kc
     C                   parm                    @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@rrn2

      /free

       If @@rrn <> ' ';
          xxRrn = %int(@@rrn);
       Endif;

       If @@rrn2 <> ' ';
          xxRrn2 = %int(@@rrn2);
       Endif;

       //*** Beg Chg ***** 09/01/25 **************************
       //*** W0042R02(@@kc:@@kl:@@mode:xxRrn:xxRrn2);
       // @@rrn is               RRN of Agency Producer File WMAPP
       @@Rrn1 = @@rrn2;       // RRN of Agency Master File WMAGP
       @@mode1 = 'D';
       @@Next1 = 'WTAGTPROD';
       @@TabInd1 = '00000';
       DriverPgm(@@Rrn1:@@Mode1:@@Next1:@@TabInd1);
       //*** End Chg ***** 09/01/25 **************************

       Return;

      /end-free
