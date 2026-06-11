     H*  (C) Copyright Progressive Data Solutions, Inc., 1997
     H/TITLE CVTWMALP1: V3 Conversion for Files WMALP, WMOOP
     fwmalp     uf   e             disk    usropn                                Entity/Location
     fwmoop     o  a e             disk    usropn                                Officers/Owners

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
     d  xxWMALOPN                     1a   inz('0')                                WMALP open flag
     d  xxWMOOOPN                     1a   inz('0')                                WMOOP open flag
     d  xxLIB                        10a                                           File library
     d  xxFILE                       10a                                           File name
     d  xxCMD                       128a                                           System command
     d  xxLENGTH                     15p 5                                         Command length

      *************************************************
      ** Officer/Owner fields based on WMALP fields  **
      *************************************************
     dooFields         ds                  based(ooPntr)                         Officer/Owner
     d  ooOLSN                       30a                                           Last name
     d  ooOFSN                       15a                                           First name
     d  ooOFPC                        5p 2                                         Percentage
     d  ooOSS#                        9a                                           Soc Sec number
     d  ooEXFL                        1a                                           Exempt flag
     d  ooEXDT                        7p 0                                         Exempt date

      *************************************************
      ** Officer/Owner basing fields from WMALP      **
      *************************************************
     dooBasing         ds                                                        Officer/Owner
     d  alOLN1                       30a                                           Last name 1
     d   alOFN1                      15a                                            First name 1
     d   alOFP1                       5p 2                                          Percentage 1
     d   alSS#1                       9a                                            Soc Sec number 1
     d   alONC1                       1a                                            Exempt flag 1
     d   alED01                       7p 0                                          Exempt date 1
     d  alOLN2                       30a                                           Last name 2
     d   alOFN2                      15a                                            First name 2
     d   alOFP2                       5p 2                                          Percentage 2
     d   alSS#2                       9a                                            Soc Sec number 2
     d   alONC2                       1a                                            Exempt flag 2
     d   alED02                       7p 0                                          Exempt date 2
     d  alOLN3                       30a                                           Last name 3
     d   alOFN3                      15a                                            First name 3
     d   alOFP3                       5p 2                                          Percentage 3
     d   alSS#3                       9a                                            Soc Sec number 3
     d   alONC3                       1a                                            Exempt flag 3
     d   alED03                       7p 0                                          Exempt date 3
     d  alOLN4                       30a                                           Last name 4
     d   alOFN4                      15a                                            First name 4
     d   alOFP4                       5p 2                                          Percentage 4
     d   alSS#4                       9a                                            Soc Sec number 4
     d   alONC4                       1a                                            Exempt flag 4
     d   alED04                       7p 0                                          Exempt date 4
     d  alOLN5                       30a                                           Last name 5
     d   alOFN5                      15a                                            First name 5
     d   alOFP5                       5p 2                                          Percentage 5
     d   alSS#5                       9a                                            Soc Sec number 5
     d   alONC5                       1a                                            Exempt flag 5
     d   alED05                       7p 0                                          Exempt date 5
     d  alOLN6                       30a                                           Last name 6
     d   alOFN6                      15a                                            First name 6
     d   alOFP6                       5p 2                                          Percentage 6
     d   alSS#6                       9a                                            Soc Sec number 6
     d   alONC6                       1a                                            Exempt flag 6
     d   alED06                       7p 0                                          Exempt date 6
     d  alOLN7                       30a                                           Last name 7
     d   alOFN7                      15a                                            First name 7
     d   alOFP7                       5p 2                                          Percentage 7
     d   alSS#7                       9a                                            Soc Sec number 7
     d   alONC7                       1a                                            Exempt flag 7
     d   alED07                       7p 0                                          Exempt date 7

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

      ** Read next Entity/Location record

     c                   read      wmalp                                  LR

      ** If not EOF, update new fields, write Officer/Owner record

     c                   if        *inLR=*off                                    >---+
                                                                                     :
      **       Create Officer/Owner records if data exists in WMALP                  :
                                                                                     :
     c                   eval      ooPntr=%addr(alOLN1)                              :
     c                   if        ooOLSN<>*blanks                               >-+ :
     c                   exsr      $MakeWMOOP                                      : :
     c                   endif                                                   <-+ :
     c                   eval      ooPntr=%addr(alOLN2)                              :
     c                   if        ooOLSN<>*blanks                               >-+ :
     c                   exsr      $MakeWMOOP                                      : :
     c                   endif                                                   <-+ :
     c                   eval      ooPntr=%addr(alOLN3)                              :
     c                   if        ooOLSN<>*blanks                               >-+ :
     c                   exsr      $MakeWMOOP                                      : :
     c                   endif                                                   <-+ :
     c                   eval      ooPntr=%addr(alOLN4)                              :
     c                   if        ooOLSN<>*blanks                               >-+ :
     c                   exsr      $MakeWMOOP                                      : :
     c                   endif                                                   <-+ :
     c                   eval      ooPntr=%addr(alOLN5)                              :
     c                   if        ooOLSN<>*blanks                               >-+ :
     c                   exsr      $MakeWMOOP                                      : :
     c                   endif                                                   <-+ :
     c                   eval      ooPntr=%addr(alOLN6)                              :
     c                   if        ooOLSN<>*blanks                               >-+ :
     c                   exsr      $MakeWMOOP                                      : :
     c                   endif                                                   <-+ :
     c                   eval      ooPntr=%addr(alOLN7)                              :
     c                   if        ooOLSN<>*blanks                               >-+ :
     c                   exsr      $MakeWMOOP                                      : :
     c                   endif                                                   <-+ :
                                                                                     :
      **       Set default values in WMALP and clear Officer/Owner fields            :
                                                                                     :
     c                   eval      alNCCI='Y'                                        :
                                                                                     :
     c                   if        alDBA<>alNAME                                 >-+ :
     c                   eval      alFDNM=alDBA                                      :
     c                   endif                                                   <---+

     c                   clear                   alDBA                               :
     c                   clear                   ooBasing                            :
     c                   update    pal01                                             :
     c                   endif                                                   <---+

      *************************************************
      ** Program termination                         **
      *************************************************
     clr                 if        xxWMALOPN=*on                                 >-+
     clr                 eval      xxFILE='WMALP'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxWMOOOPN=*on                                 >-+
     clr                 eval      xxFILE='WMOOP'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+

      *************************************************
      ** Program initialization                      **
      *************************************************
     csr   *inzsr        begsr                                                   >---+
     csr                 eval      xxWMALOPN=*off                                    :
     csr                 eval      xxWMOOOPN=*off                                    :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WMALP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWMALOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WMOOP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWMOOOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 if        xxWMALOPN=*off or xxWMOOOPN=*off              >-+ :
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
     csr                 when      xxFILE='WMALP'                                >-+ :
     csr                 close     wmalp                                91         : :
     csr                 when      xxFILE='WMOOP'                                >-+ :
     csr                 close     wmoop                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 eval      xxCMD=cnDLTOVR+%trim(xxFILE)+')'                  :
     csr                 eval      xxLENGTH=128                                      :
     csr                 call      'QCMDEXC'                            91           :
     csr                 parm                    xxCMD                               :
     csr                 parm                    xxLENGTH                            :
     csr                 endsr                                                   <---+

      *************************************************
      ** Make an Officer/Owner record                **
      *************************************************
     csr   $MakeWMOOP    begsr                                                   >-+
     csr                 eval      ooCO#=alCO#                                     :
     csr                 eval      ooFND=alFND                                     :
     csr                 eval      ooEMP#=alEMP#                                   :
     csr                 eval      ooDIV=alDIV                                     :
     csr                 eval      ooELID=alELID                                   :
     csr                 eval      ooEFDT=alEFDT                                   :
     csr                 eval      ooTITL=*blanks                                  :
     csr                 eval      ooDOB=*zeros                                    :
     csr                 eval      ooSEQ#=*zeros                                   :
     csr                 eval      ooSTAT=alSTAT                                   :
     csr                 eval      ooUSER=alUSER                                   :
     csr                 eval      ooPGM=alPGM                                     :
     csr                 eval      ooTIME=alTIME                                   :
     csr                 eval      ooDT=alDT                                       :
     csr                 eval      ooIUSR=alIUSR                                   :
     csr                 eval      ooIPGM=alIPGM                                   :
     csr                 eval      ooITIM=alITM                                    :
     csr                 eval      ooIDT=alIDT                                     :
     csr                 eval      ooCPFL=*blanks                                  :
     csr                 write     poo01                                           :
     csr                 endsr                                                   <-+

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
     csr                 when      xxFILE='WMALP'                                >-+ :
     csr                 open      wmalp                                91         : :
     csr                 when      xxFILE='WMOOP'                                >-+ :
     csr                 open      wmoop                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 endsr                                                   <---+
