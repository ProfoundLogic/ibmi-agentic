     H*  (C) Copyright Progressive Data Solutions, Inc., 1997
     H/TITLE CVTWACTP2: V3 Conversion for File WACTP
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
     i                             P    1    2 0ctCO#                            COMPANY NUMBER
     i                             P    3    4 0ctFND                            FUND NUMBER
     i                             P    5    6 0ctFYR                            FUND CENTURY AN
     i                             P    7    9 0ctEMP#@                          EMPLOYER NUMBER
     i                             P   10   11 0ctDIV@                           DIVISION
     i                             P   12   15 0ctCASE                           CASE NUMBER
     i                                 16   24  ctSS#                            SOCIAL SECURITY
     i                             P   25   27 0ctCLAS                           CLASS CODE
     i                                 28   33 0ctACDT@                          ACCIDENT DATE
     i                             P   34   35 0ctXFYR                           FUND CENTURY AN
     i                             P   36   38 0ctXEMP@                          EMPLOYER NUMBER
     i                             P   39   40 0ctXDIV@                          DIVISION
     i                             P   41   44 0ctXCAS                           CASE NUMBER
     i                                 45   53  ctXSS#                           SOCIAL SECURITY
     i                             P   54   56 0ctXCLS                           CLASS CODE
     i                                 57   62 0ctXACD@                          ACCIDENT DATE
     i                                 63   92  ctREQ                            REQUEST BY
     i                             P   93   95 0ctDET#                           # DETAIL CHANGE
     i                                 96   96  ctPRT                            PRINT FLAG
     i                                 97  106  ctUSER                           USER
     i                                107  116  ctPGM                            PROGRAM
     i                                117  122 0ctTIME                           TIME
     i                                123  128 0ctDT@                            DATE MAINTAINED
     i                                129  129  ctDLTF                           CLAIM DELETION

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
