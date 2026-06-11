     H*  (C) Copyright Progressive Data Solutions, Inc., 1998
     H/TITLE V36CVTS4: V4 Conversion for File FMS4P
     hdatedit(*ymd)
     fnmphp     if   e             disk    usropn
     fnmhsp     if   e             disk    usropn
     ffmp1l     if   e           k disk    usropn
     ffms4p     o  a e             disk    usropn

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
     d  IX                            5p 0                                         Index
     d  xxNMPHOPN                     1a   inz('0')                                NMPHP open flag
     d  xxNMHSOPN                     1a   inz('0')                                NMHSP open flag
     d  xxFMP1OPN                     1a   inz('0')                                FMP1L open flag
     d  xxFMS4OPN                     1a   inz('0')                                FMS4P open flag
     d  xxLIB                        10a                                           File library
     d  xxFILE                       10a                                           File name
     d  xxCMD                       128a                                           System command
     d  xxLENGTH                     15p 5                                         Command length
     d  xxNWCDTB                     50a                                           NWCD table
     d    xxNWCD                      5a   dim(10) overlay(xxNWCDTB)                 NWCD entry
     d    p1NW01                      5a   overlay(xxNWCDTB:1)                       p1 Code 1
     d    p1NW02                      5a   overlay(xxNWCDTB:6)                       p1 Code 2
     d    p1NW03                      5a   overlay(xxNWCDTB:11)                      p1 Code 3
     d    p1NW04                      5a   overlay(xxNWCDTB:16)                      p1 Code 4
     d    p1NW05                      5a   overlay(xxNWCDTB:21)                      p1 Code 5
     d    p1NW06                      5a   overlay(xxNWCDTB:26)                      p1 Code 6
     d    p1NW07                      5a   overlay(xxNWCDTB:31)                      p1 Code 7
     d    p1NW08                      5a   overlay(xxNWCDTB:36)                      p1 Code 8
     d    p1NW09                      5a   overlay(xxNWCDTB:41)                      p1 Code 9
     d    p1NW10                      5a   overlay(xxNWCDTB:46)                      p1 Code 10
     d  xxNINTTB                    150a                                           NINT table
     d    xxNINT                     15a   dim(10) overlay(xxNINTTB)                 NINT entry
     d    p1N#01                     15a   overlay(xxNINTTB:1)                       p1 Code 1
     d    p1N#02                     15a   overlay(xxNINTTB:16)                      p1 Code 2
     d    p1N#03                     15a   overlay(xxNINTTB:31)                      p1 Code 3
     d    p1N#04                     15a   overlay(xxNINTTB:46)                      p1 Code 4
     d    p1N#05                     15a   overlay(xxNINTTB:61)                      p1 Code 5
     d    p1N#06                     15a   overlay(xxNINTTB:76)                      p1 Code 6
     d    p1N#07                     15a   overlay(xxNINTTB:91)                      p1 Code 7
     d    p1N#08                     15a   overlay(xxNINTTB:106)                     p1 Code 8
     d    p1N#09                     15a   overlay(xxNINTTB:121)                     p1 Code 9
     d    p1N#10                     15a   overlay(xxNINTTB:136)                     p1 Code 10


      *************************************************
      ** System area                                 **
      *************************************************
     dSysArea         sds                                                        System area
     d  syPGM                  1     10a                                           Program name
     d  syUSER               254    263a                                           User name

      *************************************************
      ** Program entry processing                    **
      *************************************************
     c     *entry        plist
     c                   parm                    pmBASELIB        10
     c                   parm                    pmCLTLIB         10
     c                   parm                    pmCOMPANY         3 0

     c     fmp1Key       klist
     c                   kfld                    s4FEIN
     c                   kfld                    s4MOD

      *************************************************
      ** Mainline processing                         **
      *************************************************

      ** Process next NMPHP

     c                   if        *in91=*off                                    >-+
     c                   read      nmphp                                  91       :
     c                   endif                                                   <-+

      ** If not EOF, chain to FMP1L and create FMS4P record

     c                   if        *in91=*off                                    >-----+
     c                   clear                   ps401                                 :
     c                   eval      s4FEIN=phFEIN                                       :
     c                   eval      s4MOD=phFMOD                                        :
     c                   eval      s4NWCD=phNWCD                                       :
     c     fmp1Key       chain     fmp1l                              90               :
     c                   if        *in90=*off                                    >---+ :
     c                   eval      IX=1                                              : :
     c     phNWCD        lookup    xxNWCD(IX)                             90         : :
     c                   if        *in90=*on                                     >-+ : :
     c                   eval      s4NINT=xxNINT(IX)                               : : :
     c                   endif                                                   <-+ : :
     c                   endif                                                   <---+ :
     c                   eval      s4SYS=phFL01                                        :
     c                   eval      s4BGDT=phNEDT                                       :
     c                   eval      s4ENDT=phNXDT                                       :
     c                   eval      s4DISC=phDTCD                                       :
     c                   eval      s4FEE=phFEEC                                        :
     c                   eval      s4FSFL=phFSFL                                       :
     c                   eval      s4ECFL=*blanks                                      :
     c                   eval      s4MDFL=*blanks                                      :
     c                   eval      s4STAT=phSTAT                                       :
     c                   eval      s4USER=phUSER                                       :
     c                   eval      s4PGM=phPGM                                         :
     c                   eval      s4TIME=phTIME                                       :
     c                   eval      s4DT=phDT                                           :
     c                   eval      s4IUSR=phIUSR                                       :
     c                   eval      s4IPGM=phIPGM                                       :
     c                   eval      s4ITIM=phITIM                                       :
     c                   eval      s4IDT=phIDT                                         :
     c                   write     ps401                                               :
     c                   endif                                                   <-----+

      ** Process next NMHSP

     c                   if        *in92=*off                                    >-+
     c                   read      nmhsp                                  92       :
     c                   endif                                                   <-+

      ** If not EOF, chain to FMP1L and create FMS4P record

     c                   if        *in92=*off                                    >-----+
     c                   clear                   ps401                                 :
     c                   eval      s4FEIN=hsFEIN                                       :
     c                   eval      s4MOD=hsFMOD                                        :
     c                   eval      s4NWCD=hsNWCD                                       :
     c     fmp1Key       chain     fmp1l                              90               :
     c                   if        *in90=*off                                    >---+ :
     c                   eval      IX=1                                              : :
     c     hsNWCD        lookup    xxNWCD(IX)                             90         : :
     c                   if        *in90=*on                                     >-+ : :
     c                   eval      s4NINT=xxNINT(IX)                               : : :
     c                   endif                                                   <-+ : :
     c                   endif                                                   <---+ :
     c                   eval      s4SYS=hsFL01                                        :
     c                   eval      s4BGDT=hsCEDT                                       :
     c                   eval      s4ENDT=hsTMDT                                       :
     c                   eval      s4DISC=hsDTCD                                       :
     c                   eval      s4FEE=hsFEE                                         :
     c                   eval      s4FSFL=hsFSFL                                       :
     c                   eval      s4ECFL=hsECFL                                       :
     c                   eval      s4MDFL=hsMDFL                                       :
     c                   eval      s4STAT=hsSTAT                                       :
     c                   eval      s4USER=hsUSER                                       :
     c                   eval      s4PGM=hsPGM                                         :
     c                   eval      s4TIME=hsTIME                                       :
     c                   eval      s4DT=hsDT                                           :
     c                   eval      s4IUSR=hsIUSR                                       :
     c                   eval      s4IPGM=hsIPGM                                       :
     c                   eval      s4ITIM=hsITIM                                       :
     c                   eval      s4IDT=hsIDT                                         :
     c                   write     ps401                                               :
     c                   endif                                                   <-----+

     c                   if        *in91=*on and *in92=*on                       >-+
     c                   eval      *inLR=*on                                       :
     c                   endif                                                   <-+

      *************************************************
      ** Program termination                         **
      *************************************************
     clr                 if        xxNMPHOPN=*on                                 >-+
     clr                 eval      xxFILE='NMPHP'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxNMHSOPN=*on                                 >-+
     clr                 eval      xxFILE='NMHSP'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxFMP1OPN=*on                                 >-+
     clr                 eval      xxFILE='FMP1L'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxFMS4OPN=*on                                 >-+
     clr                 eval      xxFILE='FMS4P'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+

      *************************************************
      ** Program initialization                      **
      *************************************************
     csr   *inzsr        begsr                                                   >---+
     csr                 eval      xxNMPHOPN=*off                                    :
     csr                 eval      xxNMHSOPN=*off                                    :
     csr                 eval      xxFMP1OPN=*off                                    :
     csr                 eval      xxFMS4OPN=*off                                    :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='NMPHP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxNMPHOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='NMHSP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxNMHSOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='FMP1L'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxFMP1OPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='FMS4P'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxFMS4OPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 if        xxNMPHOPN=*off or xxNMHSOPN=*off              >-+ :
     c                             or xxFMP1OPN=*off or xxFMS4OPN=*off             : :
     csr                 eval      *inLR=*on                                       : :
     csr                 endif                                                   <-+ :
     csr                 eval      *in91=*off                                        :
     csr                 eval      *in92=*off                                        :
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
     csr                 when      xxFILE='NMPHP'                                >-+ :
     csr                 close     nmphp                                91         : :
     csr                 when      xxFILE='NMHSP'                                >-+ :
     csr                 close     nmhsp                                91         : :
     csr                 when      xxFILE='FMP1L'                                >-+ :
     csr                 close     fmp1l                                91         : :
     csr                 when      xxFILE='FMS4P'                                >-+ :
     csr                 close     fms4p                                91         : :
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
     csr                 when      xxFILE='NMPHP'                                >-+ :
     csr                 open      nmphp                                91         : :
     csr                 when      xxFILE='NMHSP'                                >-+ :
     csr                 open      nmhsp                                91         : :
     csr                 when      xxFILE='FMP1L'                                >-+ :
     csr                 open      fmp1l                                91         : :
     csr                 when      xxFILE='FMS4P'                                >-+ :
     csr                 open      fms4p                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 endsr                                                   <---+
