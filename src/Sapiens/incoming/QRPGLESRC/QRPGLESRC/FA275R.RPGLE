     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*====================================================================
     H/TITLE API to keep FADAP in sync with SADAP
     H*====================================================================
     H*  Created: 05/20/09   C:33   Project: WC1770
     H*====================================================================
     H*  Revisions:
     H*
     H*====================================================================
     Fsadap     uf a e             disk    rename(pda01:syspf)
     Ffadap     uf a e             disk    rename(pda01:mcpf )
     Fsadal     if   e           k disk    rename(pda01:syslf)
     F                                     infds(sdbfds)
     Ffadal     if   e           k disk    rename(pda01:mclf )
     F                                     infds(fdbfds)
     D*
     D/copy faCopy2d
     D*
     C     key           klist
     C                   kfld                    DACO#
     C                   kfld                    DASUB#
     C                   kfld                    DAFND
     C                   kfld                    DAFYR
     C                   kfld                    DACASE
     C                   kfld                    DASEQ#
     C                   kfld                    DAICD9
     C*
     C/copy faCopy2c
