     H*  (C) Copyright Progressive Data Solutions, Inc., 1997
     H/TITLE CVTWACTP1: V3 Conversion for File WACTP
     fzclaim    if   e           k disk                                          Claim control
     f@wactp    if   f  129        disk    usropn                                Claim Xfer Trans
     fwactp     o    e             disk    usropn                                Claim Xfer Trans

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
     d  xxWACTOPN@                    1a   inz('0')                                @WACTP open flag
     d  xxWACTOPN                     1a   inz('0')                                WACTP open flag
     d  xxLIB                        10a                                           File library
     d  xxFILE                       10a                                           File name
     d  xxCMD                       128a                                           System command
     d  xxLENGTH                     15p 5                                         Command length

      *************************************************
      ** Old WACTP file layout                       **
      *************************************************
     i@wactp    ns
     i                             p    1    2 0ctCO#
     i                             p    3    4 0ctFND
     i                             p    5    6 0ctFYR
     i                             p    7    9 0ctEMP#@
     i                             p   10   11 0ctDIV@
     i                             p   12   15 0ctCASE
     i                             a   16   24  ctSS#
     i                             p   25   27 0ctCLAS
     i                             s   28   33 0ctACDT@
     i                             p   34   35 0ctXFYR
     i                             p   36   38 0ctXEMP@
     i                             p   39   40 0ctXDIV@
     i                             p   41   44 0ctXCAS
     i                             a   45   53  ctXSS#
     i                             p   54   56 0ctXCLS
     i                             s   57   62 0ctXACD@
     i                             a   63   92  ctREQ
     i                             p   93   95 0ctDET#
     i                             a   96   96  ctPRT
     i                             a   97  106  ctUSER
     i                             a  107  116  ctPGM
     i                             s  117  122 0ctTIME
     i                             s  123  128 0ctDT@
     i                             a  129  129  ctDLTF

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

      ** Read next old Claim Transfer Transaction record

     c                   clear                   pct01
     c                   read      @wactp                                 LR

      ** If not EOF, write new Claim Transfer Transaction record

     c                   if        *inLR=*off                                    >-----+
     c                   eval      ctACDT=ctACDT@                                      :
     c                   eval      ctXACD=ctXACD@                                      :
     c                   eval      ctDT=ctDT@                                          :
     c                   eval      ctEMP#=ctEMP#@                                      :
     c                   eval      ctDIV=ctDIV@                                        :
     c                   eval      ctXEMP=ctXEMP@                                      :
     c                   eval      ctXDIV=ctXDIV@                                      :
     c                   if        ctDLTF='Y'                                    >---+ :
     c                   eval      zGRP=ctFND                                        : :
     c     GroupKey      chain     zclaim                             91             : :
     c                   if        *in91=*off                                    >-+ : :
     c                   eval      ctDLTR=zDLTR                                    : : :
     c                   endif                                                   <-+ : :
     c                   endif                                                   <---+ :
     c                   write     pct01                                               :
     c                   endif                                                   <-----+

      *************************************************
      ** Program termination                         **
      *************************************************
     clr                 if        xxWACTOPN@=*on                                >-+
     clr                 eval      xxFILE='@WACTP'                                 :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxWACTOPN=*on                                 >-+
     clr                 eval      xxFILE='WACTP'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+

      *************************************************
      ** Program initialization                      **
      *************************************************
     csr   *inzsr        begsr                                                   >---+
     csr                 eval      zCLT=pmCOMPANY                                    :
     csr                 eval      xxWACTOPN=*off                                    :
     csr                 eval      xxLIB='*LIBL'                                     :
     csr                 eval      xxFILE='@WACTP'                                   :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWACTOPN@=*on                                  : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WACTP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWACTOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 if        xxWACTOPN@=*off or xxWACTOPN=*off             >-+ :
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
     csr                 when      xxFILE='@WACTP'                               >-+ :
     csr                 close     @wactp                               91         : :
     csr                 when      xxFILE='WACTP'                                >-+ :
     csr                 close     wactp                                91         : :
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
     csr                 when      xxFILE='@WACTP'                               >-+ :
     csr                 open      @wactp                               91         : :
     csr                 when      xxFILE='WACTP'                                >-+ :
     csr                 open      wactp                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 endsr                                                   <---+
