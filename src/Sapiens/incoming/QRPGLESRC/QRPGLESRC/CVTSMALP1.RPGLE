     H*  (C) Copyright Progressive Data Solutions, Inc., 1997
     H/TITLE CVTSMALP1: V3 Conversion for File SMALP
     fzgroup    if   e           k disk
     fsmupp     if   e             disk    usropn
     fsmalp     o  a e             disk    usropn

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
     d  xxSMUPOPN                     1a   inz('0')                                SMUPP open flag
     d  xxSMALOPN                     1a   inz('0')                                SMALP open flag
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

      *************************************************
      ** Mainline processing                         **
      *************************************************

      ** Read next input record

     c                   read      smupp                                  LR

      ** If not EOF, create output record

     c                   if        *inLR=*off                                    >-+
     c                   eval      alUSR=upUSR                                     :
     c                   eval      alST=zState                                     :
     c                   eval      alLIC=upALIC                                    :
     c                   eval      alSTAT='A'                                      :
     c                   write     pal01                                           :
     c                   endif                                                   <-+

      *************************************************
      ** Program termination                         **
      *************************************************
     clr                 if        xxSMUPOPN=*on                                 >-+
     clr                 eval      xxFILE='SMUPP'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxSMALOPN=*on                                 >-+
     clr                 eval      xxFILE='SMALP'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+

      *************************************************
      ** Program initialization                      **
      *************************************************
     csr   *inzsr        begsr                                                   >---+
     csr   pmCOMPANY     chain     zgroup                             91             :
     csr                 eval      xxSMUPOPN=*off                                    :
     csr                 eval      xxSMALOPN=*off                                    :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='SMUPP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxSMUPOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='SMALP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxSMALOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 if        xxSMUPOPN=*off or xxSMALOPN=*off              >-+ :
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
     csr                 when      xxFILE='SMUPP'                                >-+ :
     csr                 close     smupp                                91         : :
     csr                 when      xxFILE='SMALP'                                >-+ :
     csr                 close     smalp                                91         : :
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
     csr                 when      xxFILE='SMUPP'                                >-+ :
     csr                 open      smupp                                91         : :
     csr                 when      xxFILE='SMALP'                                >-+ :
     csr                 open      smalp                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 endsr                                                   <---+
