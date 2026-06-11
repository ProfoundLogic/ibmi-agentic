     H*  (C) Copyright Progressive Data Solutions, Inc., 1997
     H/TITLE CVTWMDSP2: V3 Conversion for File WMDSP
     fzgroup    if   e           k disk                                          Group Control
     fwdf1l     if   e           k disk    usropn                                Fund Detail
     fwmdsp     uf   e             disk    usropn                                Discount file

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
     d  xxWMDSOPN                     1a   inz('0')                                WMDSP open flag
     d  xxWDF1OPN                     1a   inz('0')                                WDF1L open flag
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
     c                   kfld                    zGRP

     c     FundKey       klist
     c                   kfld                    dsCO#
     c                   kfld                    dsFND
     c                   kfld                    dsFYR

      *************************************************
      ** Mainline processing                         **
      *************************************************

      ** Read next Discount record and corresponding Group Control record

     c                   read      wmdsp                                  LR
     c                   if        *inLR=*off                                    >-+
     c                   eval      zGRP=dsFND                                      :
     c     GroupKey      chain     zgroup                             91           :
     c                   endif                                                   <-+

      ** If not EOF and group found, update record

     c                   if        *inLR=*off and *in91=*off                     >---+
     c                   eval      dsST=zSTATE                                       :
     c     FundKey       chain     wdf1l                              91             :
     c                   if        *in91=*off                                    >-+ :
     c                   eval      dsEFDT=f1STPO                                   : :
     c                   endif                                                   <-+ :
     c                   update    pds01                                             :
     c                   endif                                                   <---+

      *************************************************
      ** Program termination                         **
      *************************************************
     clr                 if        xxWMDSOPN=*on                                 >-+
     clr                 eval      xxFILE='WMDSP'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxWDF1OPN=*on                                 >-+
     clr                 eval      xxFILE='WDF1L'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+

      *************************************************
      ** Program initialization                      **
      *************************************************
     csr   *inzsr        begsr                                                   >---+
     csr                 eval      zCLT=pmCOMPANY                                    :
     csr                 eval      xxWMDSOPN=*off                                    :
     csr                 eval      xxWDF1OPN=*off                                    :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WMDSP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWMDSOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WDF1L'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWDF1OPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 if        xxWMDSOPN=*off or xxWDF1OPN=*off              >-+ :
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
     csr                 when      xxFILE='WMDSP'                                >-+ :
     csr                 close     wmdsp                                91         : :
     csr                 when      xxFILE='WDF1P'                                >-+ :
     csr                 close     wdf1l                                91         : :
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
     csr                 when      xxFILE='WMDSP'                                >-+ :
     csr                 open      wmdsp                                91         : :
     csr                 when      xxFILE='WDF1L'                                >-+ :
     csr                 open      wdf1l                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 endsr                                                   <---+
