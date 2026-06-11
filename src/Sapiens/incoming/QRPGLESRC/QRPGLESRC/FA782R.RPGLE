     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*******************************************************************
     H/TITLE API - FA782R - Assign Discount Code Based on License Number*
     H*******************************************************************
     H* Description:  API to Assign Discount Code Based on License      *
     H*               Number.                                           *
     H*                                                                 *
     H*  Called From:                                                   *
     H*  F0351RFL                                                       *
     H*  F0361RFL                                                       *
     H*                                                                 *
     H*  Calls To:    None                                              *
     H*                                                                 *
     H*******************************************************************
     H* Created on 04/30/2013 by C:284   for  Project 147MAS            *
     H*******************************************************************
     H* Revisions:                                                      *
     H*                                                                 *
     H* 08/10/16 C: 079 Support: 42844/114                              *
     H*                                                                 *
     H*******************************************************************
     FFAEOP     uf   e             disk
      * EOMB Transaction File
     FFTLPL1    if   e           k disk
      * License Panel Rate Code Table - Logical
     FNMNWL     if   e           k disk
      * PPO Network Master - Logical
      *
      // Procedure Defination
     DchkBillupld      pr                  extpgm('FA743R')
     D @@mode                         1
     D @@rrn                          9  0
     D ##stcd                         3
     D xxupld                         1
      *
      // StandAlone variable
     D @@mode          S              1
     D @@rrn           S              9  0
     D @@lic#          S             13
     D kklic#          S                   like(lplic#)
     D                                     inz(*blanks)
     D ##stcd          S              3
     D kkefdt          S                   like(lpefdt)
     D xxupld          S              1
      *
     C     *entry        plist
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@lic#
      *
      /Free
       Chain @@rrn peo01;
       kklic# = @@lic#;
       if %found(faeop) and eorcn = 0;
         //*** Beg Chg *** 08/10/16 ****************************************
         //**** kkefdt = 1130101;
         //**** chain (@@lic#:kkefdt) plp01;
         //**if %found(ftlpl1);
             kkefdt = EOtrdt;
             setgt (@@lic#:kkefdt) plp01;
             readpe (@@lic#) plp01;
             if %found(ftlpl1) and lpstat = 'A';
         //*** End Chg *** 08/10/16 ***************************************

                eontwk = lpntwk;
                eodt = %dec(%date:*cymd);
                eotime = %dec(%time:*hms);
                eopgm = 'FA782R';

                update peo01;

             endif;

       endif;
       *inlr = *on;
      /End-Free
