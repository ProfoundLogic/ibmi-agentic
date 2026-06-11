     H*  (C) Copyright Progressive Data Solutions, Inc., 1997
     H/TITLE CVTWDELP1: V3 Conversion for File WDELP
     fwdelp     uf   e             disk    usropn                                Employer Detail
     fwdf1l     if   e           k disk    usropn                                Fund Detail

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
     d  xxWDELOPN                     1a   inz('0')                                WDELP open flag
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

     c     f1Key         klist
     c                   kfld                    elCO#
     c                   kfld                    elFND
     c                   kfld                    elFYR

      *************************************************
      ** Mainline processing                         **
      *************************************************

      ** Read next Employer Detail record

     c                   read      wdelp                                  LR

      ** If not EOF, update new fields

     c                   if        *inLR=*off                                    >---+
     c                   eval      elBSDT=elPFDT                                     :
     c     f1Key         chain     wdf1l                              91             :
     c                   if        *in91=*off                                    >-+ :
     c                   eval      elRTCD=f1RTCD                                   : :
     c                   endif                                                   <-+ :
     c                   if        elDCDE=*blanks                                >-+ :
     c                   eval      elCXBL=*blanks                                  : :
     c                   endif                                                   <-+ :
     c                   if        elSTAT=*blanks                                >-+ :
     c                   eval      elSTAT='N'                                      : :
     c                   endif                                                   <-+ :
     c                   update    pel01                                             :
     c                   endif                                                   <---+

      *************************************************
      ** Program termination                         **
      *************************************************
     clr                 if        xxWDELOPN=*on                                 >-+
     clr                 eval      xxFILE='WDELP'                                  :
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
     csr                 eval      xxWDELOPN=*off                                    :
     csr                 eval      xxWDF1OPN=*off                                    :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WDELP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWDELOPN=*on                                   : :
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
     csr                 if        xxWDELOPN=*off or xxWDF1OPN=*off              >-+ :
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
     csr                 when      xxFILE='WDELP'                                >-+ :
     csr                 close     wdelp                                91         : :
     csr                 when      xxFILE='WDF1L'                                >-+ :
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
     csr                 when      xxFILE='WDELP'                                >-+ :
     csr                 open      wdelp                                91         : :
     csr                 when      xxFILE='WDF1L'                                >-+ :
     csr                 open      wdf1l                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 endsr                                                   <---+
