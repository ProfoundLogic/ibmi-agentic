     D*=====================================================================
     D* Run command (QCMDEXC) procedure prototype
     D*=====================================================================
     P*=====================================================================
     P* runCmd - run CL command
     P*  Parameter 1 - 3000 character (varible size)
     P*  Parameter 2 - 15,5 numeric
     P*  Returns - nothing
     P*=====================================================================
     DrunCmd           PR                  extpgm('QCMDEXC')
     D cmdStr                      3000    Const Options(*varSize)
     D cmdLength                     15  5 Const
     D
