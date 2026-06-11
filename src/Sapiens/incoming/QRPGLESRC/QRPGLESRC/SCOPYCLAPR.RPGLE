     *========================================================================*
     h*copyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h* StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description:  This copybook will be used to declare the protype of the
     h*  procedure which will load Claim action panel in Tranform
     h*
     h*  Called from: Many prograns
     h*  Calls to:    None
     h*
     h*
     h*  Created on 3/7/2012 by C:189 for Project WC2115
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     D noEMail         c                   Const('No E-Mail Available')
     D xcMedOnly       c                   Const('Med Only')
     D xcLostTime      c                   Const('Lost Time')
     D xcRecOnly       c                   Const('Record Only')
     D kkPcnt          s              1
     D date            s               d   datfmt(*usa)
     D y               s              1  0
     h*
     D* Data Structures
     D                 ds                  inz
     D  ed                     1     35  0
     D                                     Dim(5) ascend                        EFFECT. DATE
     D  EMefD1                 1      7  0
     D  EMefD2                 8     14  0
     D  EMefD3                15     21  0
     D  EMefD4                22     28  0
     D  EMefD5                29     35  0

     D                 ds                  inz
     D  cd                     1     35  0
     D                                     Dim(5) ascend                        CANCEL DATE
     D  EMcaD1                 1      7  0
     D  EMcaD2                 8     14  0
     D  EMcaD3                15     21  0
     D  EMcaD4                22     28  0
     D  EMcaD5                29     35  0

     d LodActPnlC      pr

