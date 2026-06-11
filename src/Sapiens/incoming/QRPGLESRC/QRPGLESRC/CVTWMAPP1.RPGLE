     H*  (C) Copyright Progressive Data Solutions, Inc., 1998
     H/TITLE CVTWMAPP1: V1 to V3 Conversion for File WMAPP
     fzgroup    if   e           k disk                                          Group Control
     fwmapp     uf   e             disk    usropn                                Agent Producer

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
     d  xxWMAPOPN                     1a   inz('0')                                WMAPP open flag
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

     c     GroupKey      klist
     c                   kfld                    zCLT

      *************************************************
      ** Mainline processing                         **
      *************************************************

      ** Read next Agent Producer record

     c                   read      wmapp                                  LR

      ** If not EOF, update record

     c                   if        *inLR=*off                                    >---+
     c                   if        *in96=*off                                    >-+ :
     c                   eval      apRST=zSTATE                                    : :
     c                   update    pap01                                           : :
     c                   endif                                                   <-+ :
     c                   endif                                                   <---+

      *************************************************
      ** Program termination                         **
      *************************************************
     clr                 if        xxWMAPOPN=*on                                 >-+
     clr                 eval      xxFILE='WMAPP'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+

      *************************************************
      ** Program initialization                      **
      *************************************************
     csr   *inzsr        begsr                                                   >---+
     csr                 eval      zCLT=pmCOMPANY                                    :
     csr   GroupKey      chain     zgroup                             96             :
     csr                 eval      xxWMAPOPN=*off                                    :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WMAPP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWMAPOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 if        xxWMAPOPN=*off                                >-+ :
     csr                 eval      *inLR=*on                                       : :
     csr                 endif                                                   <-+ :
     csr                 endsr                                                   <---+

      *************************************************
      ** Check for file                              **
      *************************************************
     csr   $Check        begsr                                                   >---+
     csr                 if        %subst(xxFILE:1:1)='@'                        >-+ :
     csr                 eval      xxCMD=cnCHKOBJ+%trim(xxLIB)+'/'+                : :
     c                             %trim(%subst(xxFILE:2))+cnOBJTYPE               : :
     csr                 else                                                    >-+ :
     csr                 eval      xxCMD=cnCHKOBJ+%trim(xxLIB)+'/'+                : :
     c                             %trim(xxFILE)+cnOBJTYPE                         : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLENGTH=128                                      :
     csr                 call      'QCMDEXC'                            91           :
     csr                 parm                    xxCMD                               :
     csr                 parm                    xxLENGTH                            :
     csr                 endsr                                                   <---+

      *************************************************
      ** Close a file                                **
      *************************************************
     csr   $Close        begsr                                                   >---+
     csr                 select                                                      :
     csr                 when      xxFILE='WMAPP'                                >-+ :
     csr                 close     wmapp                                91         : :
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
     csr                 if        %subst(xxFILE:1:1)='@'                        >-+ :
     csr                 eval      xxCMD=cnOVRDBF+%trim(xxFILE)+cnTOFILE+          : :
     c                             %trim(xxLIB)+'/'+%trim(%subst(xxFILE:2))+')'    : :
     csr                 else                                                    >-+ :
     csr                 eval      xxCMD=cnOVRDBF+%trim(xxFILE)+cnTOFILE+          : :
     c                             %trim(xxLIB)+'/'+%trim(xxFILE)+')'              : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLENGTH=128                                      :
     csr                 call      'QCMDEXC'                            91           :
     csr                 parm                    xxCMD                               :
     csr                 parm                    xxLENGTH                            :
     csr                 select                                                      :
     csr                 when      xxFILE='WMAPP'                                >-+ :
     csr                 open      wmapp                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 endsr                                                   <---+
