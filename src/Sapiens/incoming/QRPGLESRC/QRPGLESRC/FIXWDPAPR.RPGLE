     HCopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H DFTACTGRP(*NO) ACTGRP(*caller)
      *
      *---------------------------------------------------------------------------*
      * Created    : 03/06/15                                                     *
      * Programmer : C:180                                                        *
      * Support    : 33610                                                        *
      *---------------------------------------------------------------------------*
      *                                                                           *
      * This pgm fixes the weeks and days for selected records in the payment     *
      * file.                                                                     *
      *                                                                           *
      * The pgm takes 4 parameters:                                               *
      *  Report : Value '1' signifies print report, '0' do not print report.      *
      *  Mode   : Value '1' signifies update mode,  '0' inquiry mode.             *
      *  Date   : Date value in format CYYMMDD - Records to be processed          *
      *           from this date until eof.                                       *
      *                                                                           *
      *  Type   : Value 'M' signifies process Medical payments.                   *
      *           Value 'C' signifies process Comp Payments.                      *
      *           Value ' ' or blank signifies process other than medical or Comp.*
      *                                                                           *
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
      * 10/19/15   C:180   Check for active groups
      *
      * 09/25/15   C:180   Recalculate wks/days for payments currently
      *                    with zero wks and zero days if they have the
      *                    same from and to service dates.
      *
      * 09/23/15   C:180   Support 09/24/15
      *                    Add trans code to report.
      *
      * 08/13/15   C:180   Support 33845
      *                    Check for PV1s that exclude weeks/days calc.
      *
      * 05/12/15   C:180   Support 33845
      *                    Change report sequence - Sort by check #
      *

     FWDPAL4    UF   E           K DISK
     FWTABL     IF   E           K DISK    RENAME(PPA01:PPA01T)
     FWMFNL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     D FIXWDPAPR       PR
     D   @Prt                         1A   Const
     D   @Upd                         1A   Const
     D   @Date                        7A   Const
     D   @Type                        1A   Const
      *
     D FIXWDPAPR       PI
     D   @Prt                         1A   Const
     D   @Upd                         1A   Const
     D   @Date                        7A   Const
     D   @Type                        1A   Const
      *
     D Case#           DS
     D  pafnd
     D    dash1                       1    inz('-')
     D  pafyr
     D    dash2                       1    inz('-')
     D  pacase
      *
     D Weeks           s              5  0 INZ
     D Days            s                   Like(padays)INZ
     D Time6           S               T   INZ
     D @@trdt          s                   Like(patrdt)INZ
     D count1          s              6  0 INZ
     D count2          s              6  0 INZ
     D count3          s              6  0 INZ
     D xxfrdt          s               D   Datfmt(*MDY.) inz
     D xxtodt          s               D   Datfmt(*MDY.) inz
     D xxtrdt          s               D   Datfmt(*MDY.) inz
     D intrdt          s               D   Datfmt(*MDY.)
     D xxkey           s                   like(b2key)
     D xxfnd           s                   like(fnfnd) inz
     D xxco#           s                   like(fnco#) inz(777)
     D xxtoday         s              7  0 INZ
     D xxtype          s              7A   INZ
     D xxcltid         s              3A   INZ
     D errmsg          s             23    INZ
     D errmsg1         c                   Const('From/To date <= 0.')
     D errmsg2         c                   Const('Message available.    ')
     D errmsg3         c                   Const('Weeks > 1000.')
     D errmsg4         c                   Const('PV1 settings not found.')

     D Rtvcltid        pr                  extpgm('RTVCLTID')
     D  cltid                         3    Const

      /Free
       setll (*loval) PPA01;
       Read PPA01;
       Dow  not %eof(WDPAL4);

         if (@type = 'C' and paretp = @type) or
            (@type = 'M' and paretp = @type) or
            (@type = *blanks and paretp <> 'C' and paretp <> 'M');

          if pafnd <> xxfnd;
             chain (xxco#:pafnd) WMFNL;
             If %found(WMFNL);
                xxfnd = pafnd;
             Endif;
          Endif;

          if patrdt >= @@trdt and
             fnefdt <= @@trdt and
             (fnexdt > xxtoday or fnexdt = *zeros) and
             pafnd   = fnfnd  and fnstat <> 'I';
             reset xxfrdt;
             reset xxtodt;
             reset xxtrdt;

             if pafrdt > *zero;
                xxfrdt = %date(pafrdt:*CYMD);
             endif;

             if patodt > *zero;
                xxtodt = %date(patodt:*CYMD);
             endif;

             if patrdt > *zero;
                xxtrdt = %date(patrdt:*CYMD);
             endif;

             xxkey = *blanks;
             xxkey = pabr2;
             chain (Paco#:xxkey) PB201;

             If %found;
                if B2exwdc = 'Y';
                   Weeks   = *zero;
                   Days    = *zero;
                   *in80   = *on;
                else;

                   if pafrdt > *zero and patodt > *zero;
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
                   when pawks = Weeks and padays = Days;
                        count2 = count2 + 1;
                   when pawks <> Weeks or padays <> Days;
                        count3 = count3 + 1;
                        if *in77;
                           if *inof;
                              except hdg;
                           endif;
                           except DET;
                        endif;

                       if *in78;
                          pawks  = Weeks;
                          padays = Days;
                          update PPA01;
                       endif;
                endsl;

             else;

                if *inof;
                   except hdg;
                endif;
                except ERR;
             endif;

          endif;
        endif;

          Read PPA01;
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
        Weeks=(%div(%diff(%date(patodt:*CYMD):%date(pafrdt:*CYMD):*days)+1
                    :7));
        Days =(%rem(%diff(%date(patodt:*CYMD):%date(pafrdt:*CYMD):*days)+1
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
          @@trdt = %INT(@Date);
          intrdt = %date(@@trdt:*CYMD);
          Time6 = %time();
          xxtoday = %dec(%date():*cymd);
          if @type = 'M';
             xxtype = 'MEDICAL';
          Endif;

          rtvcltid(xxcltid);
          if xxcltid = '102';
             xxco# = 034;
          endif;

          if *in77;
             except hdg;
          endif;
        ENDSR;
      /End-Free
     OQSYSPRT   E            HDG              02
     O                                           14 'PGM:FIXWDPAPR '
     O                                           76 'FIX WEEKS/DAYS IN PAYMENTS'
     O               78                          85 '- UPDATE'
     O                       XXTYPE           +0001
     O                                          124 'PAGE:'
     O                       PAGE          Z  +0001
     O          E            HDG         1  1
     O                       UDATE         Y
     O                       Time6             +001
     O                                           72 'AS OF TRANSACTION DATE'
     O                       intrdt            +001
     O          E            HDG         1
     O                                           65 'OLD'
     O                                           72 'NEW'
     O                                           81 'OLD'
     O                                           87 'NEW'
     O                                          130 'TRAN'
     O          E            HDG         1
     O                                           11 'CASE NUMBER'
     O                                           27 'CHECK #'
     O                                           44 'FROM-DATE'
     O                                           56 'TO-DATE'
     O                                           67 'WEEKS'
     O                                           74 'WEEKS'
     O                                           82 'DAYS'
     O                                           88 'DAYS'
     O                                           95 'SEQ#'
     O                                          108 'TRANS-DATE'
     O                                          114 'TYPE'
     O                                          119 'PV1'
     O                                          130 'CODE'

     O          E            DET         1
     O                       CASE#
     O                       PACHK#        3     27
     O                       XXFRDT              44
     O                       XXTODT              56
     O                       PAWKS         J     66
     O                       WEEKS         J     74
     O                       PADAYS              80
     O                       DAYS                86
     O                       PASEQ#        J     95
     O                       XXTRDT             107
     O                       PARETP             113
     O                       PABR2              118
     O                       PATRCD             129

     O          E            ERR         1
     O                       CASE#
     O                       PACHK#        3     27
     O                       XXFRDT              44
     O                       XXTODT              56
     O                       PAWKS         J     66
     O                       WEEKS         J     74
     O                       PADAYS              80
     O                       DAYS                86
     O                       PASEQ#        J     95
     O                                        +0001 '***'
     O                       errmsg           +0001
     O                       PABR2              118
     O                       PATRCD             129
     O          E            TOT         2
     O                                              'Total Processed.......'
     O                       Count1        1   +001
     O                                           47 'OK......'
     O                       Count2        1   +001
     O               78                          73 'Fixed.........'
     O              n78                          73 'To be fixed...'
     O                       Count3        1   +001
