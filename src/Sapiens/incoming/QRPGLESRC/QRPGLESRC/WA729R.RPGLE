     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA729R - API to Determine OptaComp group
     H*****************************************************************
     H*  Description:                                                 *
     H*                                                               *
     H*    Access WCMS Fund Master File, get the group number         *
     H*    and Group Association Value back to calling program.       *
     H*                                                               *
     H*  Called From:                                                 *
     H*    F0350R01 - Work with Provider Bills update/display         *
     H*                                                               *
     H*  Calls To:                                                    *
     H*    None                                                       *
     H*                                                               *
     H*  Created:  04/16/13  C:277 Project:  114MJE                   *
     H*****************************************************************
     H*  Revisions:
     H*
     H*
     H*****************************************************************
     F* Fund Master File
     Fwmfnl     if   e           k disk
     F*
      *****************************************************************
      *PROGRAM FIELDS
      *****************************************************************
     Dkkco#            s                   Like(fnco#)
     Dkkfnd            s                   Like(fnfnd)
     C     *entry        plist
     C                   parm                    @@co#             3 0
     C                   parm                    @@fnd             3 0
     C                   parm                    ##gasc           10
     C*
     C     wmfnkey       klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C*
      /Free
         kkco# = @@co#;
         kkfnd = @@fnd;

         // Get the Associated Group

         Chain wmfnkey pfn01;
            If %found(wmfnl);
               ##gasc = fngrpasc;
            Endif;

         *inlr = *on;
         Return;

      /End-Free
