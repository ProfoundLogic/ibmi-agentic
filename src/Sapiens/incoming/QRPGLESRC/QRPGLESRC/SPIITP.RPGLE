     H copyright('Progressive Data Solutions, Inc 2001')
     H nomain
     Fsitpf     if   e             disk
     D*
     D/copy spritp
     P*=====================================================================
     P* getItpSvr - get the ITP server processing
     P*  No parameters
     P*  Returns - character field (length of parameter 10)
     P*=====================================================================
     PgetItpSvr        B                   Export
     DgetItpSvr        PI            10
     D
     DserverNam        s             10
     C                   if        getData = *off
     C                   movel     itPrcSvr      serverNam
     C                   endif
     C                   return    serverNam
     PgetItpSvr        E

     P*=====================================================================
     P* getModel - gets the ITP model for doc/flr
     P*  Parmeter 1 - Document name (12)
     P*  Parmeter 2 - Folder name (80)
     P*  Returns - character field (length of parameter 256)
     P*=====================================================================
     PgetModel         B                   Export
     DgetModel         PI           256
     D document                      12    const
     D folder                        80    const
     D
     Dmodel            s            256
     C                   if        getData = *off
     C                   eval      model = %trim(itDocStg) +
     C                             %trim(itDocDir) + %trim(folder) + '\' +
     C                             %trim(document) + '.itp'
     C                   endif
     C                   return    model
     PgetModel         E

     P*=====================================================================
     P* getDocNam- gets the ITP document for doc/flr
     P*  Parmeter 1 - Document name (12)
     P*  Parmeter 2 - Folder name (80)
     P*  Returns - character field (length of parameter 256)
     P*=====================================================================
     PgetDocNam        B                   Export
     DgetDocNam        PI           256
     D document                      12    const
     D folder                        80    const
     D
     DfileName         s            256
     C                   if        getData = *off
     C                   eval      fileName = %trim(itDocStg) +
     C                             %trim(itDocDir) + %trim(folder) + '\' +
     C                             %trim(document) + '.doc'
     C                   endif
     C                   return    fileName
     PgetDocNam        E

     P*=====================================================================
     P* getTmpDoc- gets the ITP temporary document for doc/flr
     P*  Parmeter 1 - Document name (12)
     P*  Parmeter 2 - Folder name (80)
     P*  Returns - character field (length of parameter 256)
     P*=====================================================================
     PgetTmpDoc        B                   Export
     DgetTmpDoc        PI           256
     D document                      12    const
     D folder                        80    const
     D
     DfileName         s            256
     C                   if        getData = *off
     C                   eval      fileName = %trim(itDocStg) +
     C                             %trim(itTmpDir) + %trim(folder) + '\' +
     C                             %trim(document) + '.doc'
     C                   endif
     C                   return    fileName
     PgetTmpDoc        E

     P*=====================================================================
     P* getPath - gets the ITP path for folder
     P*  Parmeter 1 - Folder name (80)
     P*  Returns - character field (length of parameter 256)
     P*=====================================================================
     PgetPath          B                   Export
     DgetPath          PI           256
     D folder                        80    const
     D
     DfileName         s            256
     C                   if        getData = *off
     C                   eval      fileName = %trim(itDocStg) +
     C                             %trim(itDocDir) + %trim(folder)
     C                   endif
     C                   return    fileName
     PgetPath          E

     P*=====================================================================
     P* getTmpPath - gets the ITP temporary path for folder
     P*  Parmeter 1 - Folder name (80)
     P*  Returns - character field (length of parameter 256)
     P*=====================================================================
     PgetTmpPath       B                   Export
     DgetTmpPath       PI           256
     D folder                        80    const
     D
     DfileName         s            256
     C                   if        getData = *off
     C                   eval      fileName = %trim(itDocStg) +
     C                             %trim(itTmpDir) + %trim(itDocDir) +
     C                             %trim(folder)
     C                   endif
     C                   return    fileName
     PgetTmpPath       E

     P*=====================================================================
     P* readFile - read the file for ITP information
     P*  No parameters
     P*  No return parameter
     P*=====================================================================
     PgetData          B
     DgetData          PI             1
     D
     DreadFlag         s              1
     C                   if        readFlag = *blank
     C     1             chain     sitpf                              99
     C                   move      'Y'           readFlag
     C                   endif
     C                   return                  *in99
     PgetData          E
