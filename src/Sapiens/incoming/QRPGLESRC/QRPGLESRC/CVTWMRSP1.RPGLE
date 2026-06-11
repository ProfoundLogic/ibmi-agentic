     H*  (C) Copyright Progressive Data Solutions, Inc., 1997
     H/TITLE CVTWMRSP1: V3 Conversion for File WMRSP
     fzgroup    if   e           k disk                                          Group Control
     fwmrsp     uf   e             disk    usropn                                Rate Set Code

      *************************************************
      ** Constants                                   **
      *************************************************
     dcnOVRDBF         c                   const('OVRDBF FILE(')                 Override command
     dcnDLTOVR         c                   const('DLTOVR FILE(')                 Delete override
     dcnCHKOBJ         c                   const('CHKOBJ OBJ(')                  Check object
     dcnTOFILE         c                   const(') TOFILE(')                    To file
     dcnOBJTYPE        c                   const(') OBJTYPE(*FILE)')             Object type

      *************************************************
      ** Work/Storage area                           **
      *************************************************
     dWorkStor         ds                  inz                                   Work/Storage
     d  xxWMRSOPN                     1a   inz('0')                                WMRSP open flag
     d  xxLIB                        10a                                           File library
     d  xxFILE                       10a                                           File name
     d  xxCMD                       128a                                           System command
     d  xxLENGTH                     15p 5                                         Command length

      *************************************************
      ** Program entry processing                    **
      *************************************************
     c     *entry        plist
     c                   parm                    pmBASELIB        10
     c                   parm                    pmCLTLIB         10
     c                   parm                    pmCOMPANY         3 0

      *************************************************
      ** Mainline processing                         **
      *************************************************

      ** Read next Rate Code Set record

     c                   read      wmrsp                                  LR

      ** If not EOF, update record

     c                   if        *inLR=*off                                    >-+
     c                   eval      rsJRST=zSTATE                                   :
     c                   update    prs01                                           :
     c                   endif                                                   <-+

      *************************************************
      ** Program termination                         **
      *************************************************
     clr                 if        xxWMRSOPN=*on                                 >-+
     clr                 eval      xxFILE='WMRSP'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+

      *************************************************
      ** Program initialization                      **
      *************************************************
     csr   *inzsr        begsr                                                   >---+
     csr                 eval      zCLT=pmCOMPANY                                    :
     csr   zCLT          chain     zgroup                             91             :
     csr                 eval      xxWMRSOPN=*off                                    :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WMRSP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWMRSOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 if        xxWMRSOPN=*off                                >-+ :
     csr                 eval      *inLR=*on                                       : :
     csr                 endif                                                   <-+ :
     csr                 endsr                                                   <---+

      *************************************************
      ** Check for file                              **
      *************************************************
     csr   $Check        begsr                                                   >-+
     csr                 eval      xxCMD=cnCHKOBJ+%trim(xxLIB)+'/'+                :
     c                             %trim(xxFILE)+cnOBJTYPE                         :
     csr                 eval      xxLENGTH=128                                    :
     csr                 call      'QCMDEXC'                            91         :
     csr                 parm                    xxCMD                             :
     csr                 parm                    xxLENGTH                          :
     csr                 endsr                                                   <-+

      *************************************************
      ** Close a file                                **
      *************************************************
     csr   $Close        begsr                                                   >---+
     csr                 select                                                      :
     csr                 when      xxFILE='WMRSP'                                >-+ :
     csr                 close     wmrsp                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 eval      xxCMD=cnDLTOVR+%trim(xxFILE)+')'                  :
     csr                 eval      xxLENGTH=128                                      :
     csr                 call      'QCMDEXC'                            91           :
     csr                 parm                    xxCMD                               :
     csr                 parm                    xxLENGTH                            :
     csr                 endsr                                                   <---+

      *************************************************
      ** Open a file                                 **
      *************************************************
     csr   $Open         begsr                                                   >---+
     csr                 eval      xxCMD=cnOVRDBF+%trim(xxFILE)+cnTOFILE+            :
     c                             %trim(xxLIB)+'/'+%trim(xxFILE)+')'                :
     csr                 eval      xxLENGTH=128                                      :
     csr                 call      'QCMDEXC'                            91           :
     csr                 parm                    xxCMD                               :
     csr                 parm                    xxLENGTH                            :
     csr                 select                                                      :
     csr                 when      xxFILE='WMRSP'                                >-+ :
     csr                 open      wmrsp                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 endsr                                                   <---+
