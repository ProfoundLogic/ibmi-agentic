     H*  (C) Copyright Progressive Data Solutions, Inc., 1997
     H/TITLE CVTWMBXP1: V3 Conversion for File WMBXP
     fwmfnl     if   e           k disk    usropn                                V3 Fund master
     f@wmbxp    if   f   49        disk    usropn                                V1 Billing Aux
     fwmbxp     o  a e             disk    usropn                                V3 Billing Aux

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
     d  xxWMFNOPN                     1a   inz('0')                                WMFNL open flag
     d  xxWMBXOPN@                    1a   inz('0')                                @WMBXP open flag
     d  xxWMBXOPN                     1a   inz('0')                                WMBXP open flag
     d  xxLIB                        10a                                           File library
     d  xxFILE                       10a                                           File name
     d  xxCMD                       128a                                           System command
     d  xxLENGTH                     15p 5                                         Command length

      *************************************************
      ** WMBXP V1 file definition                    **
      *************************************************
     i@wmbxp    ns
     i                             p    1    2 0bxCO#
     i                             a    3    3  bxTYPE
     i                             p    4    6 0bxEFDT
     i                             a    7   16  bxBSCD
     i                             a   17   17  bxSTAT
     i                             a   18   27  bxUSER
     i                             a   28   37  bxPGM
     i                             s   38   43 0bxTIME
     i                             s   44   49 0bxDT@

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

      ** Read next V1 Billing Auxiliary record

     c                   read      @wmbxp                                 LR

      ** If not EOF, create a V3 Billing Auxiliary record for each fund in this company

     c                   if        *inLR=*off                                    >---+
     c     bxCO#         setll     wmfnl                                             :
     c     bxCO#         reade     wmfnl                                  91         :
     c                   dow       *in91=*off                                    >-+ :
     c                   eval      bxDT=bxDT@                                      : :
     c                   eval      bxFND=fnFND                                     : :
     c                   eval      bxIUSR=*blanks                                  : :
     c                   eval      bxIPGM=*blanks                                  : :
     c                   eval      bxITM=*zeros                                    : :
     c                   eval      bxIDT=*zeros                                    : :
     c                   write     pbx01                                           : :
     c     bxCO#         reade     wmfnl                                  91       : :
     c                   enddo                                                   <-+ :
     c                   endif                                                   <---+

      *************************************************
      ** Program termination                         **
      *************************************************
     clr                 if        xxWMBXOPN@=*on                                >-+
     clr                 eval      xxFILE='@WMBXP'                                 :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxWMBXOPN=*on                                 >-+
     clr                 eval      xxFILE='WMBXP'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxWMFNOPN=*on                                 >-+
     clr                 eval      xxFILE='WMFNL'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+

      *************************************************
      ** Program initialization                      **
      *************************************************
     csr   *inzsr        begsr                                                   >---+
     csr                 eval      xxWMBXOPN@=*off                                   :
     csr                 eval      xxWMBXOPN=*off                                    :
     csr                 eval      xxWMFNOPN=*off                                    :
     csr                 eval      xxLIB='*LIBL'                                     :
     csr                 eval      xxFILE='@WMBXP'                                   :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWMBXOPN@=*on                                  : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WMBXP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWMBXOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WMFNL'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWMFNOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 if        xxWMBXOPN@=*off or xxWMBXOPN=*off             >-+ :
     c                             or xxWMFNOPN=*off                               : :
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
     csr                 when      xxFILE='@WMBXP'                               >-+ :
     csr                 close     @wmbxp                               91         : :
     csr                 when      xxFILE='WMBXP'                                >-+ :
     csr                 close     wmbxp                                91         : :
     csr                 when      xxFILE='WMFNL'                                >-+ :
     csr                 close     wmfnl                                91         : :
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
     csr                 when      xxFILE='@WMBXP'                               >-+ :
     csr                 open      @wmbxp                               91         : :
     csr                 when      xxFILE='WMBXP'                                >-+ :
     csr                 open      wmbxp                                91         : :
     csr                 when      xxFILE='WMFNL'                                >-+ :
     csr                 open      wmfnl                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 endsr                                                   <---+
