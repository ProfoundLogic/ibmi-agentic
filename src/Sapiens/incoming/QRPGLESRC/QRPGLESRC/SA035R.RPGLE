     H* (C) Copyright Progressive Data Solutions, 2001
     H***************************************************************
     H*  Created:                                                   *
     H*             03/15/01  C:69                                  *
     H*  Revisions:                                                 *
     H*                                                             *
     H***************************************************************
     Fsitpf     if   e             disk
     F
     Duse              s              1
     DprcsServer       s             10
     DdocStgSvr        s             50
     DdocDir           s             50
     DtempDir          s             50
     D*
     C     *entry        plist
     C                   parm                    use
     C                   parm                    prcsServer
     C                   parm                    docStgSvr
     C                   parm                    docDir
     C                   parm                    tempDir
     C
     C                   read      sitpf                                  99
     C                   if        *in99 = *off
     C                   movel     itUse         use
     C                   if        %parms > 1
     C                   movel     itPrcSvr      prcsServer
     C                   movel     itDocStg      docStgSvr
     C                   movel     itDocDir      docDir
     C                   movel     itTmpDir      tempDir
     C                   endif
     C                   endif
     C
     C                   seton                                        lr
