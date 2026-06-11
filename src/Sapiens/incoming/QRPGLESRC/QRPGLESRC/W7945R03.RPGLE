     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*CALLER)
     H*/TITLE W7945R03  -
    H*********************************************************************
    H*  Description: Set lock on WXCKP file  for EFT/ACH processing      *
    H*                                                                   *
    H*  Called from: W7945C                                              *
    H*                                                                   *
    H*  Calls to:                                                        *
    H*                                                                   *
    H*  Created: C:180  Support: 82888   Dated: 11/08/13                 *
    H**************************************************************************************
    H*  Revisions:                                                                        *
     H*                                                                                    *
     H*

     FWXCKP     uf   e             disk
     D JobIn           S             10    INZ('W1266C')
     DUpdEftLck        PR            10
     D JobName                       10    Const
      /Free
       UpdEftLck(JobIn);
       *inlr = *on;
      /End-Free
    P*====================================================================
     P* Update EFT lock
    P*====================================================================
     PUpdEftLck        b                   export
    P*
     DUpdEftLck        PI            10
     D JobName                       10    Const
      /Free
        Read WXCKP;
        if not %eof;
           ckjob = JobName;
           update PCK01;
        endif;
        return JobName;
      /End-Free

     PUpdEftLck        e
