     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*====================================================================
     H/TITLE API to keep FMDIP in sync with SMDIP
     H*====================================================================
     H*  Created: 05/20/09   C:33   Project: WC1770
     H*====================================================================
     H*  Revisions:
     H*
     H*  05/18/10   C:198  Project: WC1879
     H*                    Re-compiled.
     H*
     H*====================================================================
     Fsmdip     uf a e             disk    rename(pdi01:syspf)
     Ffmdip     uf a e             disk    rename(pdi01:mcpf )
     Fsmdil     if   e           k disk    rename(pdi01:syslf)
     F                                     infds(sdbfds)
     Ffmdil     if   e           k disk    rename(pdi01:mclf )
     F                                     infds(fdbfds)
     D*
     D/copy faCopy2d
     D*
     C     key           klist
     C                   kfld                    DIICD9
     C                   kfld                    DILOST
     C*
     C/copy faCopy2c
