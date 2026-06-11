     H*  (C) Copyright Progressive Data Solutions, Inc., 1997
     H/TITLE CVTWARPP2: V3 Conversion for File WARPP
     fwarpp     uf   e             disk    usropn                                Request Pymt Trans
     fwmcxl     if   e           k disk    usropn                                Claim Control Def
     fwtpsl     if   e           k disk    usropn                                Pymt Status Code

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
     d  xxWARPOPN                     1a   inz('0')                                WARPP open flag
     d  xxWMCXOPN                     1a   inz('0')                                WMCXL open flag
     d  xxWTPSOPN                     1a   inz('0')                                WTPSL open flag
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

     c     cxKey         klist
     c                   kfld                    rpCO#
     c                   kfld                    rpFND

     c     tpsKey        klist
     c                   kfld                    rpCO#
     c                   kfld                    psKEY

      *************************************************
      ** Mainline processing                         **
      *************************************************

      ** Read next Request Payment Transaction record

     c                   read      warpp                                  LR

      ** If not EOF, update new fields

     c                   if        *inLR=*off                                    >---+
     c                   eval      rpSENT='N'                                        :
     c     cxKey         chain     wmcxl                              91             :
     c                   select                                                      :
     c                   when      *in91=*off and rpSTAT='P'                     >-+ :
     c                   eval      rpSTS=cxPMTS                                    : :
     c                   when      *in91=*off                                    >-+ :
     c                   eval      rpSTS=cxPSC2                                    : :
     c                   endsl                                                   <-+ :
     c                   eval      psKEY=rpSTS                                       :
     c     tpsKey        chain     wtpsl                              91             :
     c                   if        *in91=*off                                    >-+ :
     c                   eval      rpSORT=psSRT                                    : :
     c                   endif                                                   <-+ :
     c                   eval      rpBUL1=*zero                                      :
     c                   eval      rpRQST=rpUSER                                     :
     c                   update    prp01                                             :
     c                   endif                                                   <---+

      *************************************************
      ** Program termination                         **
      *************************************************
     clr                 if        xxWARPOPN=*on                                 >-+
     clr                 eval      xxFILE='WARPP'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxWMCXOPN=*on                                 >-+
     clr                 eval      xxFILE='WMCXL'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxWTPSOPN=*on                                 >-+
     clr                 eval      xxFILE='WTPSL'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+

      *************************************************
      ** Program initialization                      **
      *************************************************
     csr   *inzsr        begsr                                                   >---+
     csr                 eval      xxWARPOPN=*off                                    :
     csr                 eval      xxWMCXOPN=*off                                    :
     csr                 eval      xxWTPSOPN=*off                                    :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WARPP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWARPOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WMCXL'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWMCXOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WTPSL'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWTPSOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 if        xxWARPOPN=*off or xxWMCXOPN=*off              >-+ :
     c                             or xxWTPSOPN=*off                               : :
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
     csr                 when      xxFILE='WARPP'                                >-+ :
     csr                 close     warpp                                91         : :
     csr                 when      xxFILE='WMCXL'                                >-+ :
     csr                 close     wmcxl                                91         : :
     csr                 when      xxFILE='WTPSL'                                >-+ :
     csr                 close     wtpsl                                91         : :
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
     csr                 when      xxFILE='WARPP'                                >-+ :
     csr                 open      warpp                                91         : :
     csr                 when      xxFILE='WMCXL'                                >-+ :
     csr                 open      wmcxl                                91         : :
     csr                 when      xxFILE='WTPSL'                                >-+ :
     csr                 open      wtpsl                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 endsr                                                   <---+
