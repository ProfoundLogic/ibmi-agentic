     H*  (C) Copyright Progressive Data Solutions, Inc., 1997
     H/TITLE CVTWMCMP1: V3 Conversion for Files WMCMP and WMLLP
     fzgroup    if   e           k disk
     fwmcmp     uf   e             disk    usropn                                Claim Master
     fwmcdl     if   e           k disk    usropn                                Claiment Demogrphic
     fwmllp     o  a e             disk    usropn                                Claim Legal

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
     d  xxWMCMOPN                     1a   inz('0')                                WMCMP open flag
     d  xxWMCDOPN                     1a   inz('0')                                WMCDL open flag
     d  xxWMLLOPN                     1a   inz('0')                                WMLLL open flag
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

     c     zKey          klist
     c                   kfld                    zCLT
     c                   kfld                    zGRP

      *************************************************
      ** Mainline processing                         **
      *************************************************

      ** Read next Claim Master record

     c                   read      wmcmp                                  LR

      ** If not EOF, update new fields, write Claim Legal record

     c                   if        *inLR=*off                                    >---+
     c                   eval      cmIDT=cmENTD                                      :
                                                                                     :
      **       Create Claim Legal if cmDEFA or cmPLAA not zero                       :
                                                                                     :
     c                   if        cmDEFA<>*zero or cmPLAA<>*zero                >-+ :
     c                   exsr      $MakeWMLLP                                      : :
     c                   endif                                                   <-+ :
                                                                                     :
      **       Get needed info from other files                                      :
                                                                                     :
     c                   clear                   cdLSN                               :
     c                   clear                   cdFSN                               :
     c                   clear                   cdMID                               :
     c     cmSS#         chain     wmcdl                              91             :
                                                                                     :
      **       Set default values in WMCMP and clear unused fields                   :
                                                                                     :
     c                   eval      cmOFF#='    1'                                    :
     c                   eval      cmLSN=cdLSN                                       :
     c                   eval      cmFSN=cdFSN                                       :
     c                   eval      cmMID=cdMID                                       :
     c                   eval      cmLSCV='11'                                       :
     c                   eval      cmEADR='Y'                                        :
     c                   if        cmLTFL='Y'                                    >-+   :
     c                   eval      cmLTFL='I'                                      :   :
     c                   else                                                    >-+   :
     c                   eval      cmLTFL='M'                                      :   :
     c                   endif                                                   <-+   :
     c                   eval      zGRP=cmFND                                          :
     c     zKey          chain     zgroup                             91               :
     c                   if        *in91=*off                                    >-+   :
     c                   eval      cmJRST=zSTATE                                   :   :
     c                   endif                                                   <-+   :
     c                   update    pcm01                                             :
     c                   endif                                                   <---+

      *************************************************
      ** Program termination                         **
      *************************************************
     clr                 if        xxWMCMOPN=*on                                 >-+
     clr                 eval      xxFILE='WMCMP'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxWMCDOPN=*on                                 >-+
     clr                 eval      xxFILE='WMCDL'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxWMLLOPN=*on                                 >-+
     clr                 eval      xxFILE='WMLLP'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+

      *************************************************
      ** Program initialization                      **
      *************************************************
     csr   *inzsr        begsr                                                   >---+
     csr                 eval      zCLT=pmCOMPANY                                    :
     csr                 eval      xxWMCMOPN=*off                                    :
     csr                 eval      xxWMCDOPN=*off                                    :
     csr                 eval      xxWMLLOPN=*off                                    :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WMCMP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWMCMOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WMCDL'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWMCDOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WMLLP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWMLLOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 if        xxWMCMOPN=*off or xxWMCDOPN=*off              >-+ :
     c                             or xxWMLLOPN=*off                               : :
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
     csr                 when      xxFILE='WMCMP'                                >-+ :
     csr                 close     wmcmp                                91         : :
     csr                 when      xxFILE='WMCDL'                                >-+ :
     csr                 close     wmcdl                                91         : :
     csr                 when      xxFILE='WMLLP'                                >-+ :
     csr                 close     wmllp                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 eval      xxCMD=cnDLTOVR+%trim(xxFILE)+')'                  :
     csr                 eval      xxLENGTH=128                                      :
     csr                 call      'QCMDEXC'                            91           :
     csr                 parm                    xxCMD                               :
     csr                 parm                    xxLENGTH                            :
     csr                 endsr                                                   <---+

      *************************************************
      ** Make a Claim Legal record                   **
      *************************************************
     csr   $MakeWMLLP    begsr                                                   >-+
     csr                 clear                   pll01                             :
     csr                 eval      llCO#=cmCO#                                     :
     csr                 eval      llFND=cmFND                                     :
     csr                 eval      llFYR=cmFYR                                     :
     csr                 eval      llCASE=cmCASE                                   :
     csr                 eval      llDEFA=cmDEFA                                   :
     csr                 eval      llDEFM=cmDEFM                                   :
     csr                 eval      llPLAA=cmPLAA                                   :
     csr                 eval      llPLAM=cmPLAM                                   :
     csr                 eval      llSTAT=cmSTAT                                   :
     csr                 eval      llUSER=cmUSER                                   :
     csr                 eval      llPGM=cmPGM                                     :
     csr                 eval      llTIME=cmTIME                                   :
     csr                 eval      llDT=cmDT                                       :
     csr                 eval      llIUSR=cmIUSR                                   :
     csr                 eval      llIPGM=cmIPGM                                   :
     csr                 eval      llITM=cmITM                                     :
     csr                 eval      llIDT=cmIDT                                     :
     csr                 write     pll01                                           :
     csr                 endsr                                                   <-+

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
     csr                 when      xxFILE='WMCMP'                                >-+ :
     csr                 open      wmcmp                                91         : :
     csr                 when      xxFILE='WMCDL'                                >-+ :
     csr                 open      wmcdl                                91         : :
     csr                 when      xxFILE='WMLLP'                                >-+ :
     csr                 open      wmllp                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 endsr                                                   <---+
