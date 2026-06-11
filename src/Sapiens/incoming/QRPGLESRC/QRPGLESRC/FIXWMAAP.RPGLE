     H*  (C) Copyright Progressive Data Solutions, Inc., 1998
     H/TITLE FIXWMAAP: V2 to V3 Conversion fix for File WMAAP
     fwmaap     uf   e             disk    usropn                                Agent Assignment
     fwmagl     if   e           k disk    usropn                                Agent Master
     fqsysprt   o    f  132        printer oflind(*in71)                         Status

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
     d  xxWMAAOPN                     1a   inz('0')                                WMAAP open flag
     d  xxWMAGOPN                     1a   inz('0')                                WMAGL open flag
     d  xxLIB                        10a                                           File library
     d  xxFILE                       10a                                           File name
     d  xxCMD                       128a                                           System command
     d  xxLENGTH                     15p 5                                         Command length
     d  xxCTINPT                      9p 0                                         Records read
     d  xxCTPRCS                      9p 0                                         Records processed
     d  xxCTOUTP                      9p 0                                         Records written

      *************************************************
      ** Program entry processing                    **
      *************************************************
     c     *entry        plist
     c                   parm                    pmBASELIB        10
     c                   parm                    pmCLTLIB         10
     c                   parm                    pmCOMPANY         3 0

     c     kyWMAG        klist
     c                   kfld                    aaCO#
     c                   kfld                    aaFEIN
     c                   kfld                    aaMOD

      *************************************************
      ** Mainline processing                         **
      *************************************************

      ** Read next Request Payment Transaction record

     c                   read      wmaap                                  LR

      ** If not EOF, update if needed

     c                   if        *inLR=*off                                    >-----+
     c                   eval      xxCTINPT=xxCTINPT+1                                 :
     c                   if        aaNAME=*blanks                                >---+ :
     c                   eval      xxCTPRCS=xxCTPRCS+1                               : :
     c     kyWMAG        chain     wmagl                              91             : :
     c                   if        *in91=*off                                    >-+ : :
     c                   eval      aaNAME=agNAME                                   : : :
     c                   eval      xxCTOUTP=xxCTOUTP+1                             : : :
     c                   update    paa01                                           : : :
     c                   endif                                                   <-+ : :
     c                   endif                                                   <---+ :
     c                   endif                                                   <-----+

      *************************************************
      ** Program termination                         **
      *************************************************
     clr                 eval      *in71=*on
     clr                 if        *in71=*on                                     >-+
     clr                 except    ##Header                                        :
     clr                 eval      *in71=*off                                      :
     clr                 endif                                                   <-+
     clr                 except    ##Totals
     clr                 if        xxWMAAOPN=*on                                 >-+
     clr                 eval      xxFILE='WMAAP'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxWMAGOPN=*on                                 >-+
     clr                 eval      xxFILE='WMAGL'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+

      *************************************************
      ** Program initialization                      **
      *************************************************
     csr   *inzsr        begsr                                                   >---+
     csr                 eval      xxWMAAOPN=*off                                    :
     csr                 eval      xxWMAGOPN=*off                                    :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WMAAP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWMAAOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WMAGL'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWMAGOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 if        xxWMAAOPN=*off or xxWMAGOPN=*off              >-+ :
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
     csr                 when      xxFILE='WMAAP'                                >-+ :
     csr                 close     wmaap                                91         : :
     csr                 when      xxFILE='WMAGL'                                >-+ :
     csr                 close     wmagl                                91         : :
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
     csr                 when      xxFILE='WMAAP'                                >-+ :
     csr                 open      wmaap                                91         : :
     csr                 when      xxFILE='WMAGL'                                >-+ :
     csr                 open      wmagl                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 endsr                                                   <---+

      *************************************************
      ** Printer output                              **
      *************************************************
     oqsysprt   e            ##Header       1  2
     o                                           79 'WMAAP Conversion Fix'
     o          e            ##Header       2
     o                                          128 'Page'
     o                       PAGE          4    132

     o          e            ##Totals       1
     o                                           22 '     Records read:'
     o                       xxCTINPT      P     35
     o          e            ##Totals       2
     o                                           22 'With blank AANAME:'
     o                       xxCTPRCS      P     35
     o          e            ##Totals       1
     o                                           22 '  Records updated:'
     o                       xxCTOUTP      P     35

