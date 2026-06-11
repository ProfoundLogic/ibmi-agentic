     D*=====================================================================
     D* ITP handling procedure prototypes
     D*=====================================================================
     P*=====================================================================
     P* getItpSvr - get the ITP server processing
     P*  No parameters
     P*  Returns - character field (length of parameter 10)
     P*=====================================================================
     DgetItpSvr        PR            10
     D
     P*=====================================================================
     P* getModel - gets the ITP model for doc/flr
     P*  Parmeter 1 - Document name (12)
     P*  Parmeter 2 - Folder name (80)
     P*  Returns - character field (length of parameter 256)
     P*=====================================================================
     DgetModel         PR           256
     D document                      12    const
     D folder                        80    const
     D
     P*=====================================================================
     P* getDocNam - gets the ITP document for doc/flr
     P*  Parmeter 1 - Document name (12)
     P*  Parmeter 2 - Folder name (80)
     P*  Returns - character field (length of parameter 256)
     P*=====================================================================
     DgetDocNam        PR           256
     D document                      12    const
     D folder                        80    const
     D
     P*=====================================================================
     P* getTmpDoc - gets the ITP temporary document for doc/flr
     P*  Parmeter 1 - Document name (12)
     P*  Parmeter 2 - Folder name (80)
     P*  Returns - character field (length of parameter 256)
     P*=====================================================================
     DgetTmpDoc        PR           256
     D document                      12    const
     D folder                        80    const
     D
     P*=====================================================================
     P* getPath  - gets the ITP path for folder
     P*  Parmeter 1 - Folder name (80)
     P*  Returns - character field (length of parameter 256)
     P*=====================================================================
     DgetPath          PR           256
     D folder                        80    const
     D
     P*=====================================================================
     P* getTmpPath  - gets the ITP temporary path for folder
     P*  Parmeter 1 - Folder name (80)
     P*  Returns - character field (length of parameter 256)
     P*=====================================================================
     DgetTmpPath       PR           256
     D folder                        80    const
     D
     P*=====================================================================
     P* getData - read the file for ITP information
     P*  No parameters
     P*  Returns indicator
     P*=====================================================================
     DgetData          PR             1
     D
