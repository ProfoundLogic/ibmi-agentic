     h*  (C) Copyright Progressive Data Solutions, Inc., 1997
     h/TITLE CVTFMPDP1: V3 Conversion for Files FMPDP and WMPDP
     ffmp1p     if   e             disk    usropn                                Input file
     ffmpdl     uf   e           k disk    usropn prefix(f) rename(ppd01:fppd01) Payee Demographics
     fwmpdl     uf   e           k disk    usropn prefix(w) rename(ppd01:wppd01) Payee Demographics

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
     d  xxFMP1OPN                     1a   inz('0')                                FMP1P open flag
     d  xxFMPDOPN                     1a   inz('0')                                FMPDL open flag
     d  xxWMPDOPN                     1a   inz('0')                                WMPDL open flag
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

     c     fmp1Key       klist
     c                   kfld                    p1FEIN
     c                   kfld                    p1MOD

      *************************************************
      ** Mainline processing                         **
      *************************************************

      ** Read next Input record

     c                   read      fmp1p                                  LR

      ** If not EOF, chain and update

     c                   if        *inLR=*off                                    >---+
     c     fmp1Key       chain     fmpdl                              91             :
     c                   if        *in91=*off and p1LIC#<>fpdFLIC                >-+ :
     c                   eval      fpdFLIC=p1LIC#                                  : :
     c                   update    fppd01                                          : :
     c                   endif                                                   <-+ :
     c     fmp1Key       chain     wmpdl                              91             :
     c                   if        *in91=*off and p1LIC#<>wpdFLIC                >-+ :
     c                   eval      wpdFLIC=p1LIC#                                  : :
     c                   update    wppd01                                          : :
     c                   endif                                                   <-+ :
     c                   endif                                                   <---+

      *************************************************
      ** Program termination                         **
      *************************************************
     clr                 if        xxFMP1OPN=*on                                 >-+
     clr                 eval      xxFILE='FMP1P'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxFMPDOPN=*on                                 >-+
     clr                 eval      xxFILE='FMPDL'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxWMPDOPN=*on                                 >-+
     clr                 eval      xxFILE='WMPDL'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+

      *************************************************
      ** Program initialization                      **
      *************************************************
     csr   *inzsr        begsr                                                   >---+
     csr                 eval      xxFMP1OPN=*off                                    :
     csr                 eval      xxFMPDOPN=*off                                    :
     csr                 eval      xxWMPDOPN=*off                                    :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='FMP1P'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxFMP1OPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxFILE='FMPDL'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxFMPDOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxFILE='WMPDL'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWMPDOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 if        xxFMP1OPN=*off or xxFMPDOPN=*off              >-+ :
     c                             or xxWMPDOPN=*off                               : :
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
     csr                 when      xxFILE='FMP1P'                                >-+ :
     csr                 close     fmp1p                                91         : :
     csr                 when      xxFILE='FMPDL'                                >-+ :
     csr                 close     fmpdl                                91         : :
     csr                 when      xxFILE='WMPDL'                                >-+ :
     csr                 close     wmpdl                                91         : :
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
     csr                 when      xxFILE='FMP1P'                                >-+ :
     csr                 open      fmp1p                                91         : :
     csr                 when      xxFILE='FMPDL'                                >-+ :
     csr                 open      fmpdl                                91         : :
     csr                 when      xxFILE='WMPDL'                                >-+ :
     csr                 open      wmpdl                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 endsr                                                   <---+
