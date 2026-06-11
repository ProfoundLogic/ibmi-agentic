     HCopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H DFTACTGRP(*NO) ACTGRP(*caller)
      *
      *---------------------------------------------------------------------------*
      * Created    : 04/08/15                                                     *
      * Programmer : C:180                                                        *
      * Support    : 33610                                                        *
      *---------------------------------------------------------------------------*
      *                                                                           *
      * This pgm fixes the weeks and days for selected records in the payment     *
      * file.                                                                     *
      *                                                                           *
      * The pgm takes 3 parameters:                                               *
      *  Report : Value '1' signifies print report, '0' do not print report.      *
      *  Mode   : Value '1' signifies update mode,  '0' inquiry mode.             *
      *  Type   : Value 'M' signifies process Medical payments.                   *
      *           Value 'C' signifies process Comp Payments.                      *
      *           Value ' ' or blank signifies process other than medical or Comp.*
      *---------------------------------------------------------------------------*
      * Revisions:
      * 12/14/15   C:180   Support 33833
      *                    Subset by Comp.
      *
      * 11/10/15   C:180   Support 33833
      *                    Add PV1 to error message.
      *
      * 11/06/15   C:180   Support 38200
      *                    Add parameter to run by type (M=Medical, C= Comp
      *
      * 09/25/15   C:180   Recalculate wks/days for payments currently
      *                    with zero wks and zero days if they have
      *                    same from and to service dates.
      *
      * 09/24/15   C:180   Support 37048
      *                    Add transaction code to report.
      *
      * 08/13/15   C:180   Support 33845
      *                    Check for PV1s that exclude weeks/days calc.
      *
      *

     FWARPP     UF   E           K DISK
     FWTABL     IF   E           K DISK    RENAME(PPA01:PPA01T)
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     D FIXWARPPR       PR
     D   @Prt                         1A   Const
     D   @Upd                         1A   Const
     D   @Type                        1A   Const
      *
     D FIXWARPPR       PI
     D   @Prt                         1A   Const
     D   @Upd                         1A   Const
     D   @Type                        1A   Const
      *
     D Case#           DS
     D  rpfnd
     D    dash1                       1    inz('-')
     D  rpfyr
     D    dash2                       1    inz('-')
     D  rpcase
      *
     D Weeks           s              5  0 INZ
     D Days            s                   Like(rpdays)INZ
     D Time6           S               T   INZ
     D count1          s              6  0 INZ
     D count2          s              6  0 INZ
     D count3          s              6  0 INZ
     D xxfrdt          s               D   Datfmt(*MDY.) inz
     D xxtodt          s               D   Datfmt(*MDY.) inz
     D xxtrdt          s               D   Datfmt(*MDY.) inz
     D xxkey           s                   like(b2key)
     D xxtype          s              7A   INZ
     D errmsg          s             23    INZ
     D errmsg1         c                   Const('From/To date <= 0.')
     D errmsg2         c                   Const('Message available     .')
     D errmsg3         c                   Const('Weeks > 1000.')
     D errmsg4         c                   Const('PV1 settings not found.')
      /Free
       Read PRP01;
       Dow  not %eof(WARPP);


         if (@type = 'C' and rpretp = @type) or
            (@type = 'M' and rpretp = @type) or
            (@type = *blanks and rpretp <> 'C' and rpretp <> 'M');
             reset xxfrdt;
             reset xxtodt;
             reset xxtrdt;

             if rpfrdt > *zero;
                xxfrdt = %date(rpfrdt:*CYMD);
             endif;

             if rptodt > *zero;
                xxtodt = %date(rptodt:*CYMD);
             endif;

             if rptrdt > *zero;
                xxtrdt = %date(rptrdt:*CYMD);
             endif;

             xxkey = *blanks;
             xxkey = rpbr2;
             chain (RPco#:xxkey) PB201;

             If %found;
                if B2exwdc = 'Y';
                   Weeks   = *zero;
                   Days    = *zero;
                   *in80   = *on;
                else;

                   if rpfrdt > *zero and rptodt > *zero;
                      exsr CalcWks;
                   else;
                      *in80 = *off;
                      errmsg = errmsg1;
                   endif;

                endif;

             else;
                *in80 = *off;
                errmsg = errmsg4;
             endif;

             if *in80;
                count1 = count1 + 1;

                select;
                   when rpwks = Weeks and rpdays = Days;
                        count2 = count2 + 1;
                   when rpwks <> Weeks or rpdays <> Days;
                        count3 = count3 + 1;
                        if *in77;
                           if *inof;
                              except hdg;
                           endif;
                           except DET;
                        endif;

                        if *in78;
                           rpwks  = Weeks;
                           rpdays = Days;
                           update PRP01;
                        endif;
                endsl;

             else;

                if *inof;
                   except hdg;
                endif;
                   except ERR;
             endif;
          endif;

         Read PRP01;
       ENDDO;

       if *in77;
          if *inof;
             except hdg;
          endif;
          except tot;
       endif;

       *inlr = *on;
       //--------------------------------------------------------------------------
       //- Calculate Wks and days                                                 -
       //--------------------------------------------------------------------------
        Begsr CalcWks;
        Weeks=(%div(%diff(%date(rptodt:*CYMD):%date(rpfrdt:*CYMD):*days)+1
                    :7));
        Days =(%rem(%diff(%date(rptodt:*CYMD):%date(rpfrdt:*CYMD):*days)+1
                    :7));
        if Weeks < 1000;
           *in80=*on;
        else;
           *in80=*off;
           errmsg = errmsg3;
        endif;
        Endsr;
       //--------------------------------------------------------------------------
       //- Initialization                                                         -
       //--------------------------------------------------------------------------
        Begsr *inzsr;
          *in77 = @Prt;
          *in78 = @Upd;
          Time6 = %time();
          if @type = 'M';
             xxtype = 'MEDICAL';
          Endif;
          if *in77;
             except hdg;
          endif;
        ENDSR;
      /End-Free
     OQSYSPRT   E            HDG              02
     O                                           14 'PGM:FIXWARPPR '
     O                                           76 'FIX WEEKS/DAYS IN PAYMENTS'
     O               78                          85 '- UPDATE'
     O                       XXTYPE           +0001
     O                                          124 'PAGE:'
     O                       PAGE          Z  +0001
     O          E            HDG         1  1
     O                       UDATE         Y
     O                       Time6             +001
     O          E            HDG         1
     O                                           65 'OLD'
     O                                           72 'NEW'
     O                                           81 'OLD'
     O                                           87 'NEW'
     O                                          130 'TRAN'
     O          E            HDG         1
     O                                           11 'CASE NUMBER'
     O                                           27 'SEQ #  '
     O                                           44 'FROM-DATE'
     O                                           56 'TO-DATE'
     O                                           67 'WEEKS'
     O                                           74 'WEEKS'
     O                                           82 'DAYS'
     O                                           88 'DAYS'
     O                                          108 'TRANS-DATE'
     O                                          114 'TYPE'
     O                                          119 'PV1'
     O                                          130 'CODE'
     O          E            DET         1
     O                       CASE#
     O                       RPSEQ#        3     27
     O                       XXFRDT              44
     O                       XXTODT              56
     O                       rpWKS         J     66
     O                       WEEKS         J     74
     O                       rpDAYS              80
     O                       DAYS                86
     O                       XXTRDT             107
     O                       RPRETP             113
     O                       RPBR2              118
     O                       RPTRCD             129

     O          E            ERR         1
     O                       CASE#
     O                       RPSEQ#        3     27
     O                       XXFRDT              44
     O                       XXTODT              56
     O                       rpWKS         J     66
     O                       WEEKS         J     74
     O                       rpDAYS              80
     O                       DAYS                86
     O                                        +0001 '***'
     O                       errmsg           +0001
     O                       RPBR2              118
     O                       RPTRCD             129
     O          E            TOT         2
     O                                              'Total Processed.......'
     O                       Count1        1   +001
     O                                           47 'OK......'
     O                       Count2        1   +001
     O               78                          73 'Fixed.........'
     O              n78                          73 'To be fixed...'
     O                       Count3        1   +001
