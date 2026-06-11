     H*  (C) Copyright Progressive Data Solutions, Inc., 1997
     H/TITLE CVTWDECP1: V3 Conversion for File WDECP
     fzgroup    if   e           k disk                                          Group Control
     fwdf1p     if   e             disk    usropn                                Fund Detail
     fwdecp     o  a e             disk    usropn                                Expnse Constant Def

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
     d  xxWDECOPN                     1a   inz('0')                                WDECP open flag
     d  xxWDF1OPN                     1a   inz('0')                                WDF1P open flag
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

      *************************************************
      ** Mainline processing                         **
      *************************************************

      ** Read next Fund Detail and corresponding Group Control record

     c                   read      wdf1p                                  LR
     c                   if        *inLR=*off                                    >-+
     c                   eval      zGRP=f1FND                                      :
     c     GroupKey      chain     zgroup                             LR           :
     c                   endif                                                   <-+

      ** If not EOF, create an Expense Constant Definition record

     c                   if        *inLR=*off                                    >---+
     c                   clear                   pec01                               :
     c                   eval      ecCO#=f1CO#                                       :
     c                   eval      ecFND=f1FND                                       :
     c                   eval      ecEFDT=f1STPO                                     :
     c                   eval      ecST=zSTATE                                       :
     c                   eval      ecMPEC=f1MPEC                                     :
     c                   eval      ecEXCO=f1EXCO                                     :
     c                   write     pec01                                             :
                                                                                     :
      ** If F1EXC2 is not 0, create another Expense Constant Definition record       :
                                                                                     :
     c                   if        f1EXC2<>*zero                                 >-+ :
     c                   eval      ecCO#=f1CO#                                     : :
     c                   eval      ecFND=f1FND                                     : :
     c                   eval      ecEFDT=f1XDT2                                   : :
     c                   eval      ecST=zSTATE                                     : :
     c                   eval      ecMPEC=f1MPEC                                   : :
     c                   eval      ecEXCO=f1EXC2                                   : :
     c                   write     pec01                                           : :
     c                   endif                                                   <-+ :
     c                   endif                                                   <---+

      *************************************************
      ** Program termination                         **
      *************************************************
     clr                 if        xxWDECOPN=*on                                 >-+
     clr                 eval      xxFILE='WDECP'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxWDF1OPN=*on                                 >-+
     clr                 eval      xxFILE='WDF1P'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+

      *************************************************
      ** Program initialization                      **
      *************************************************
     csr   *inzsr        begsr                                                   >---+
     csr                 eval      zCLT=pmCOMPANY                                    :
     csr                 eval      xxWDECOPN=*off                                    :
     csr                 eval      xxWDF1OPN=*off                                    :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WDECP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWDECOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WDF1P'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWDF1OPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 if        xxWDECOPN=*off or xxWDF1OPN=*off              >-+ :
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
     csr                 when      xxFILE='WDECP'                                >-+ :
     csr                 close     wdecp                                91         : :
     csr                 when      xxFILE='WDF1P'                                >-+ :
     csr                 close     wdf1p                                91         : :
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
     csr                 when      xxFILE='WDECP'                                >-+ :
     csr                 open      wdecp                                91         : :
     csr                 when      xxFILE='WDF1P'                                >-+ :
     csr                 open      wdf1p                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 endsr                                                   <---+
