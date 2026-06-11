     H*  (C) Copyright Progressive Data Solutions, Inc., 1997
     H/TITLE CVTWDRBP1: V3 Conversion for File WDRBP
     fzgroup    if   e           k disk                                          Group Control
     fwdf1p     if   e           k disk    usropn                                Fund Detail
     fwdrbp     o  a e             disk    usropn                                Comp Reimbursement

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
     d  xxWDF1OPN                     1a   inz('0')                                WDF1P open flag
     d  xxWDRBOPN                     1a   inz('0')                                WDRBP open flag
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

      ** If not EOF, create a Compensation Reimbursement record

     c                   if        *inLR=*off                                    >---+
     c                   clear                   prb01                               :
     c                   eval      rbCO#=f1CO#                                       :
     c                   eval      rbFND=f1FND                                       :
     c                   eval      rbST=zSTATE                                       :
     c                   eval      rbEFDT=f1STPO                                     :
     c                   eval      rbREPC=f1REPC                                     :
     c                   eval      rbPACP=f1PACP                                     :
     c                   eval      rbMNRT=20                                         :
     c                   write     prb01                                             :
                                                                                     :
      ** If F1EFDT is not 0, create another Compensation Reimbursement record        :
                                                                                     :
     c                   if        f1EFDT<>*zero                                 >-+ :
     c                   eval      rbCO#=f1CO#                                     : :
     c                   eval      rbFND=f1FND                                     : :
     c                   eval      rbST=zSTATE                                     : :
     c                   eval      rbEFDT=f1EFDT                                   : :
     c                   eval      rbREPC=f1PCP2                                   : :
     c                   clear                   rbPACP                            : :
     c                   eval      rbMNRT=20                                       : :
     c                   write     prb01                                           : :
     c                   endif                                                   <-+ :
     c                   endif                                                   <---+

      *************************************************
      ** Program termination                         **
      *************************************************
     clr                 if        xxWDF1OPN=*on                                 >-+
     clr                 eval      xxFILE='WDF1P'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxWDRBOPN=*on                                 >-+
     clr                 eval      xxFILE='WDRBP'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+

      *************************************************
      ** Program initialization                      **
      *************************************************
     csr   *inzsr        begsr                                                   >---+
     csr                 eval      zCLT=pmCOMPANY                                    :
     csr                 eval      xxWDF1OPN=*off                                    :
     csr                 eval      xxWDRBOPN=*off                                    :
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
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WDRBP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWDRBOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 if        xxWDF1OPN=*off or xxWDRBOPN=*off              >-+ :
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
     csr                 when      xxFILE='WDF1P'                                >-+ :
     csr                 close     wdf1p                                91         : :
     csr                 when      xxFILE='WDRBP'                                >-+ :
     csr                 close     wdrbp                                91         : :
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
     csr                 when      xxFILE='WDF1P'                                >-+ :
     csr                 open      wdf1p                                91         : :
     csr                 when      xxFILE='WDRBP'                                >-+ :
     csr                 open      wdrbp                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 endsr                                                   <---+
