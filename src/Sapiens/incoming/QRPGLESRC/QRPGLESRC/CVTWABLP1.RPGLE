     H*  (C) Copyright Progressive Data Solutions, Inc., 1997
     H/TITLE CVTWABLP1: V3 Conversion for File WABLP
     fwablp     uf   e             disk    usropn                                Bill Transaction
     fwmahl     if   e           k disk    usropn                                Agent History
     fwdell     if   e           k disk    usropn                                Employer Detail

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
     d  xxWABLOPN                     1a   inz('0')                                WABLP open flag
     d  xxWMAHOPN                     1a   inz('0')                                WMAHL open flag
     d  xxWDELOPN                     1a   inz('0')                                WDELL open flag
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

     c     ahelKey       klist
     c                   kfld                    blCO#
     c                   kfld                    blFND
     c                   kfld                    blEMP#
     c                   kfld                    blDIV
     c                   kfld                    blFYR

      *************************************************
      ** Mainline processing                         **
      *************************************************

      ** Read next Bill Transaction record

     c                   read      wablp                                  LR

      ** If not EOF, update new fields

     c                   if        *inLR=*off                                    >---+
     c     ahelKey       setgt     wmahl                                             :
     c                   dou       *in91=*on or ahSTAT<>'I'                      >-+ :
     c     ahelKey       readpe    wmahl                                  91       : :
     c                   enddo                                                   <-+ :
     c                   if        *in91=*off                                    >-+ :
     c                   eval      blAGT#=ahAGT#                                   : :
     c                   endif                                                   <-+ :
     c     ahelKey       chain     wdell                              91             :
     c                   if        *in91=*off                                    >-+ :
     c                   eval      blPFDT=elPFDT                                   : :
     c                   endif                                                   <-+ :
     c                   update    pbl01                                             :
     c                   endif                                                   <---+

      *************************************************
      ** Program termination                         **
      *************************************************
     clr                 if        xxWABLOPN=*on                                 >-+
     clr                 eval      xxFILE='WABLP'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxWMAHOPN=*on                                 >-+
     clr                 eval      xxFILE='WMAHL'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxWDELOPN=*on                                 >-+
     clr                 eval      xxFILE='WDELL'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+

      *************************************************
      ** Program initialization                      **
      *************************************************
     csr   *inzsr        begsr                                                   >---+
     csr                 eval      xxWABLOPN=*off                                    :
     csr                 eval      xxWMAHOPN=*off                                    :
     csr                 eval      xxWDELOPN=*off                                    :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WABLP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWABLOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WMAHL'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWMAHOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WDELL'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWDELOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 if        xxWABLOPN=*off or xxWMAHOPN=*off              >-+ :
     c                             or xxWDELOPN=*off                               : :
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
     csr                 when      xxFILE='WABLP'                                >-+ :
     csr                 close     wablp                                91         : :
     csr                 when      xxFILE='WMAHL'                                >-+ :
     csr                 close     wmahl                                91         : :
     csr                 when      xxFILE='WDELL'                                >-+ :
     csr                 close     wdell                                91         : :
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
     csr                 when      xxFILE='WABLP'                                >-+ :
     csr                 open      wablp                                91         : :
     csr                 when      xxFILE='WMAHL'                                >-+ :
     csr                 open      wmahl                                91         : :
     csr                 when      xxFILE='WDELL'                                >-+ :
     csr                 open      wdell                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 endsr                                                   <---+
