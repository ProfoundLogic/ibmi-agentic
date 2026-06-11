     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
      *      ***************** WARNING *****************************
      *
      *             This program is no longer in use.  It simply
      *             calls STAPPCFG.  Do not make any changes to
      *             this program anymore.
      *
      *             C:65  Project: SY251   Date: 03/16/15
      *
      *      ***************** WARNING *****************************
     h/title WTAPPCFG - Application Configuration(Profound)
     h dftname(WTAPPCFG)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h/define   profoundUI
     h bnddir('WTBNDDIR')
     h**********************************************************************
      *===============================================================*
      *  Created   : 04/05/14                                         *
      *  Programmer: C:279                                            *
      *  Project   : WC2197                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *
      *
      *
      *      ***************** WARNING *****************************
      *
      *             This program is no longer in use.  It simply
      *             calls STAPPCFG.  Do not make any changes to
      *             this program anymore.
      *
      *             C:65  Project: SY251   Date: 03/16/15
      *
      *      ***************** WARNING *****************************
      *
      *
      *
      *
      *
      *  12/01/14 C:300 Project: 114WLF                               *
      *                 Modified to use WTACTPNL record format        *
      *                 recSysApp for action panel.                   *
      *                                                               *
      *  12/04/14 C:242 Project 114WGU                                *
      *                 Add a new tab to configure different Visit    *
      *                 Types for Visit Tracking.                     *
      *                 The action panel is replaced by the company   *
      *                 level version from WTACTPNL                   *
      *                                                               *
      *  10/02/14 C:65  Project SY251                                 *
      *                 Add a new tab to program for Documents Setup, *
      *                 the scriptura revamp of Department letters.   *
      *                 The action panel is replaced by the company   *
      *                 level version from WTACTPNL.                  *
      *                                                               *
      *  09/09/14 C:295 Project WC2247 Priss: 12                      *
      *                 The action panel should have a same           *
      *                 colour through out in the new PV1 Code screen.*
      *                 (For compilation Only)                        *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*


      * Entry of Program.

     c     *entry        plist
      ***** beg chg ***** 12/01/14 ******************************************
     c******             parm                    @@mode            1            mode
     c******             parm                    @@TabInd          5            tab index
     c                   parm                    @@rrn             9            tab index
     c                   parm                    @@mode            1            tab index
     c                   parm                    @@next           10            tab index
     c                   parm                    @@TabInd          5            tab index
      ***** end chg ***** 12/01/14 ******************************************

      **** begin add **** 03/16/15 ******************************************
     c                   Call      'STAPPCFG'
     c                   parm                    @@rrn             9            tab index
     c                   parm                    @@mode            1            tab index
     c                   parm                    @@next           10            tab index
     c                   parm                    @@TabInd          5            tab index
      **** end add ****** 03/16/15 ******************************************

      /free
       *InLr = *On;
      /end-free
