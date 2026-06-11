     HCopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H DFTACTGRP(*NO) ACTGRP(*caller)
      *
      *---------------------------------------------------------------------------*
      * Created    : 09/11/15                                                     *
      * Programmer : C:180                                                        *
      * Support    : 33930                                                        *
      *---------------------------------------------------------------------------*
      *                                                                           *
      * This pgm fixes orphan ICD records from file SADAP.                        *
      *                                                                           *
      * The pgm takes 3 parameters:                                               *
      *  Report : Value '1' signifies print report, '0' do not print report.      *
      *  Mode   : Value '1' signifies update mode,  '0' inquiry mode.             *
      *  Date   : Date value in format CYYMMDD - Records to be processed          *
      *           from this date until eof.                                       *
      *                                                                           *
      *---------------------------------------------------------------------------*
      * Revisions:
      *
      *

     FSADAP     UF   E             DISK
     FWMCML1    IF   E           K DISK
     FWACTL2    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
      *
     D FIXICDR         PR
     D   @Prt                         1A   Const
     D   @Upd                         1A   Const
     D   @Date                        7A   Const
      *
     D FIXICDR         PI
     D   @Prt                         1A   Const
     D   @Upd                         1A   Const
     D   @Date                        7A   Const
      *
     D Time6           S               T   INZ
     D xxtrdt          s               D   Datfmt(*MDY.)
     D count1          S              6  0 INZ
     D count2          S              6  0 INZ
     D count3          S              6  0 INZ
     D LOVAL           S              7  0 INZ(1)
     D Errmsg          S             30A   INZ
      *
     D Errmsg1         C                   Const('Case reference not found.')
     D Errmsg2         C                   Const('New case number not found.')


     D IcdCase         DS
     D  dafnd
     D    dash1                       1    inz('-')
     D  dafyr
     D    dash2                       1    inz('-')
     D  dacase

     D NewCase         DS
     D  CMfnd
     D    Ndash1                      1    inz('-')
     D  CMfyr
     D    Ndash2                      1    inz('-')
     D  CMcase

      /Free
       setll LOVAL PDA01;
       Read PDA01;
       Dow  not %eof(SADAP);
          Chain (daco#:dafnd:dafyr:dacase) PCM01;
          If not %found(WMCML1);
             Count1 = Count1 + 1;
             Chain (daco#:dafnd:dafyr:dacase) PCT01;
             If %found(WACTL2);
                Chain (ctco#:ctfnd:ctfyr:ctcase) PCM01;
                If %found(WMCML1);
                    Exsr Process;
                Else;
                    Errmsg = Errmsg2;
                    Exsr Error;
                Endif;
             Else;
                 Errmsg = Errmsg1;
                 Exsr Error;
             Endif;
          Endif;
          Read PDA01;
       Enddo;

       if *in77;
          if *inof;
             except hdg;
          endif;
          except tot;
       endif;

       *inlr = *on;
       //--------------------------------------------------------------------------
       // Process                                                               -
       //--------------------------------------------------------------------------
        Begsr Process;
        Count2 = Count2 + 1;

        if *in77;
           if *inof;
              except HDG;
           endif;
           except DET;
        endif;

        If *in78;
           DAcase = CMcase;
           DAfyr  = CMfyr;
           Update PDA01;
        Endif;

        Endsr;
       //--------------------------------------------------------------------------
       // Error                                                                 -
       //--------------------------------------------------------------------------
        Begsr Error;
        Count3 = Count3 + 1;

        if *in77;
           if *inof;
              except HDG;
           endif;
           except ERR;
        endif;

        Endsr;
       //--------------------------------------------------------------------------
       // Initialization                                                         -
       //--------------------------------------------------------------------------
        Begsr *inzsr;
          *in77 = @Prt;
          *in78 = @Upd;
          xxtrdt = %date(%INT(@Date):*CYMD);
          Time6 = %time();
          if *in77;
             except hdg;
          endif;
        ENDSR;
      /End-Free
     OQSYSPRT   E            HDG            1 02
     O                                              'PGM:FIXICDR'
     O                       UDATE         Y   +002
     O                       Time6             +001
     O                                           77 'FIX ORPHAN ICD RECORDS'
     O               78                        +001 '- UPDATE'
     O                                          124 'PAGE:'
     O                       PAGE          Z  +0001
     O          E            HDG         1
     O                                           30 'SEQ #'
     O                                           46 'ORPHAN ICD'
     O                                           62 'NEW ICD'
     O          E            HDG         1
     O                                           47 'CASE NUMBER'
     O                                           66 'CASE NUMBER'

     O          E            DET         1
     O                       DASEQ#        1     30
     O                       IcdCase             51
     O                       NewCase             70

     O          E            ERR         2  1
     O                       IcdCase             51
     O                                        +0001 '*** Error:'
     O                       Errmsg           +0001

     O          E            TOT         2
     O                                              'Total Processed.......'
     O                       Count1        1   +001
     O               78                          49 'Fixed.........'
     O              n78                          49 'To be fixed...'
     O                       Count2        1   +001
     O                                           90 'Errors.........'
     O                       Count3        1   +001
