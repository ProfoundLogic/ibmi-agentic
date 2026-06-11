     H*  (C) Copyright Progressive Data Solutions, Inc., 1997
     H/TITLE CVTWMEMP2: V3 Conversion for Files WMEMP and WMOOP
     f@wmemp    if   f 1202        disk    usropn                                Old Employee Master
     fwmemp     o  a e             disk    usropn                                New Employee Master
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
     d  xxWMEMOPN@                    1a   inz('0')                                @WMEMP open flag
     d  xxWMEMOPN                     1a   inz('0')                                WMEMP open flag
     d  xxWMOOOPN                     1a   inz('0')                                WMOOP open flag
     d  xxLIB                        10a                                           File library
     d  xxFILE                       10a                                           File name
     d  xxCMD                       128a                                           System command
     d  xxLENGTH                     15p 5                                         Command length

      *************************************************
      ** Officer/Owner fields based on WMEMP fields  **
      *************************************************
     dooFields         ds                  based(ooPntr)                         Officer/Owner
     d  ooOLSN                       30a                                           Last name
     d  ooOFSN                       15a                                           First name
     d  ooOFPC                        5p 2                                         Percentage
     d  ooOSS#                        9a                                           Soc Sec number
     d  ooEXFL                        1a                                           Exempt flag
     d  ooEFDT                        7p 0                                         Exempt date

      *************************************************
      ** Officer/Owner basing fields from WMEMP      **
      *************************************************
     dooBasing         ds                                                        Officer/Owner
     d  emOLN1                       30a                                           Last name 1
     d   emOFN1                      15a                                            First name 1
     d   emOFP1                       5p 2                                          Percentage 1
     d   emSS#1                       9a                                            Soc Sec number 1
     d   emONC1                       1a                                            Exempt flag 1
     d   emED01                       7p 0                                          Exempt date 1
     d  emOLN2                       30a                                           Last name 2
     d   emOFN2                      15a                                            First name 2
     d   emOFP2                       5p 2                                          Percentage 2
     d   emSS#2                       9a                                            Soc Sec number 2
     d   emONC2                       1a                                            Exempt flag 2
     d   emED02                       7p 0                                          Exempt date 2
     d  emOLN3                       30a                                           Last name 3
     d   emOFN3                      15a                                            First name 3
     d   emOFP3                       5p 2                                          Percentage 3
     d   emSS#3                       9a                                            Soc Sec number 3
     d   emONC3                       1a                                            Exempt flag 3
     d   emED03                       7p 0                                          Exempt date 3
     d  emOLN4                       30a                                           Last name 4
     d   emOFN4                      15a                                            First name 4
     d   emOFP4                       5p 2                                          Percentage 4
     d   emSS#4                       9a                                            Soc Sec number 4
     d   emONC4                       1a                                            Exempt flag 4
     d   emED04                       7p 0                                          Exempt date 4
     d  emOLN5                       30a                                           Last name 5
     d   emOFN5                      15a                                            First name 5
     d   emOFP5                       5p 2                                          Percentage 5
     d   emSS#5                       9a                                            Soc Sec number 5
     d   emONC5                       1a                                            Exempt flag 5
     d   emED05                       7p 0                                          Exempt date 5
     d  emOLN6                       30a                                           Last name 6
     d   emOFN6                      15a                                            First name 6
     d   emOFP6                       5p 2                                          Percentage 6
     d   emSS#6                       9a                                            Soc Sec number 6
     d   emONC6                       1a                                            Exempt flag 6
     d   emED06                       7p 0                                          Exempt date 6
     d  emOLN7                       30a                                           Last name 7
     d   emOFN7                      15a                                            First name 7
     d   emOFP7                       5p 2                                          Percentage 7
     d   emSS#7                       9a                                            Soc Sec number 7
     d   emONC7                       1a                                            Exempt flag 7
     d   emED07                       7p 0                                          Exempt date 7

      ********************************************************
      ** Old Employee Master File                           **
      ********************************************************
     i@wmemp    ns
     i                             P    1    2 0EMCO#                            COMPANY NUMBER
     i                             P    3    4 0EMFND                            FUND NUMBER
     i                             P    5    7 0EMEMP#@                          EMPLOYER NUMBER
     i                             P    8    9 0EMDIV@                           DIVISION
     i                                 10   49  EMNAME                           NAME
     i                                 50   89  EMMAD1                           MAILING ADDRESS
     i                                 90  129  EMMAD2                           MAILING ADDRESS
     i                                130  154  EMMCTY                           MAILING CITY
     i                                155  156  EMMST                            MAILING STATE
     i                             P  157  161 0EMMZIP                           MAILING ZIP COD
     i                                162  201  EMPAD1                           PHYSICAL ADDRES
     i                                202  241  EMPAD2                           PHYSICAL ADDRES
     i                                242  266  EMPCTY                           PHYSICAL CITY
     i                                267  268  EMPST                            PHYSICAL STATE
     i                             P  269  273 0EMPZIP                           PHYSICAL ZIP CO
     i                             P  274  275 0EMAREA                           AREA CODE
     i                             P  276  279 0EMPHN#                           PHONE NUMBER
     i                             P  280  281 0EMRGN#                           REGION NUMBER
     i                             P  282  286 0EMFEIN                           FEDERAL TAX ID
     i                                287  292 0EMEFD1@                          EFFECTIVE DATE
     i                                293  298 0EMEFD2@                          EFFECTIVE DATE
     i                                299  304 0EMEFD3@                          EFFECTIVE DATE
     i                                305  310 0EMEFD4@                          EFFECTIVE DATE
     i                                311  316 0EMEFD5@                          EFFECTIVE DATE
     i                                317  322 0EMCAD1@                          CANCELLATION DA
     i                                323  328 0EMCAD2@                          CANCELLATION DA
     i                                329  334 0EMCAD3@                          CANCELLATION DA
     i                                335  340 0EMCAD4@                          CANCELLATION DA
     i                                341  346 0EMCAD5@                          CANCELLATION DA
     i                                347  386  EMCONT                           CONTACT PERSON
     i                                387  387  EMVIP                            VIP FLAG
     i                                388  388  EMMPFL                           MAILING/ PHYSIC
     i                                389  389  EMSTRP                           STATE REPORTS F
     i                                390  429  EMATTN                           ATTENTION NAME
     i                                430  469  EMDBA                            DOING BUSINESS
     i                                470  529  EMCOM1                           SPECIAL INSTRUC
     i                                530  589  EMCOM2                           SPECIAL INSTRUC
     i                                590  590  EMSTAT                           STATUS
     i                                591  600  EMUSER                           USER
     i                                601  610  EMPGM                            PROGRAM
     i                                611  616 0EMTIME                           TIME
     i                                617  622 0EMDT@                            DATE MAINTAINED
     i                             P  623  627 2EMINDE                           INITIAL DEPOSIT
     i                                628  633 0EMLIDT@                          LAST INVOICE DA
     i                             P  634  638 0EMBUL1                           UPDATE LOG NUMB
     i                                639  698  EMCOM3                           SPECIAL INSTRUC
     i                                699  728  EMOLN1                           OFFICERS LAST N
     i                                729  758  EMOLN2                           OFFICERS LAST N
     i                                759  788  EMOLN3                           OFFICERS LAST N
     i                                789  818  EMOLN4                           OFFICERS LAST N
     i                                819  848  EMOLN5                           OFFICERS LAST N
     i                                849  878  EMOLN6                           OFFICERS LAST N
     i                                879  908  EMOLN7                           OFFICERS LAST N
     i                                909  923  EMOFN1                           OFFICERS FIRST
     i                                924  938  EMOFN2                           OFFICERS FIRST
     i                                939  953  EMOFN3                           OFFICERS FIRST
     i                                954  968  EMOFN4                           OFFICERS FIRST
     i                                969  983  EMOFN5                           OFFICERS FIRST
     i                                984  998  EMOFN6                           OFFICERS FIRST
     i                                999 1013  EMOFN7                           OFFICERS FIRST
     i                               1014 1014  EMONC1                           OWN. EXEMPT.1 Y
     i                               1015 1015  EMONC2                           OWN. EXEMPT.2 Y
     i                               1016 1016  EMONC3                           OWN. EXEMPT.3 Y
     i                               1017 1017  EMONC4                           OWN. EXEMPT.4 Y
     i                               1018 1018  EMONC5                           OWN. EXEMPT.5 Y
     i                               1019 1019  EMONC6                           OWN. EXEMPT.6 Y
     i                               1020 1020  EMONC7                           OWN. EXEMPT.7 Y
     i                               1021 1026 0EMED01@                          OWN. EXEMPT.1 D
     i                               1027 1032 0EMED02@                          OWN. EXEMPT.2 D
     i                               1033 1038 0EMED03@                          OWN. EXEMPT.3 D
     i                               1039 1044 0EMED04@                          OWN. EXEMPT.4 D
     i                               1045 1050 0EMED05@                          OWN. EXEMPT.5 D
     i                               1051 1056 0EMED06@                          OWN. EXEMPT.6 D
     i                               1057 1062 0EMED07@                          OWN. EXEMPT.7 D
     i                               1063 1063  EMTPOB                           TYPE OF BUSINES
     i                               1064 1103  EMNAOB                           NATURE OF BUSIN
     i                             P 1104 1106 2EMOFP1                           OFFICERS PERCEN
     i                             P 1107 1109 2EMOFP2                           OFFICERS PERCEN
     i                             P 1110 1112 2EMOFP3                           OFFICERS PERCEN
     i                             P 1113 1115 2EMOFP4                           OFFICERS PERCEN
     i                             P 1116 1118 2EMOFP5                           OFFICERS PERCEN
     i                             P 1119 1121 2EMOFP6                           OFFICERS PERCEN
     i                             P 1122 1124 2EMOFP7                           OFFICERS PERCEN
     i                             P 1125 1128 0EMSEMP@                          STATE EMPLOYER
     i                               1129 1137  EMSS#1                           SOC. SEC. # FOR
     i                               1138 1146  EMSS#2                           SOC. SEC. # FOR
     i                               1147 1155  EMSS#3                           SOC. SEC. # FOR
     i                               1156 1164  EMSS#4                           SOC. SEC. # FOR
     i                               1165 1173  EMSS#5                           SOC. SEC. # FOR
     i                               1174 1182  EMSS#6                           SOC. SEC. # FOR
     i                               1183 1191  EMSS#7                           SOC. SEC. # FOR
     i                               1192 1192  EMSRPR                           SHORT RATE/ PRO
     i                               1193 1194  EMCNTY                           COUNTY CODE
     i                               1195 1195  EMSPAC                           SPECIAL ACCEPTA
     i                               1196 1196  EMWALS                           WATCH LIST FLAG
     i                             P 1197 1198 0EMCHAP                           CHAPTER
     i                               1199 1202  EMSIC@                           SIC CODE

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

      ** Read next old Employee record

     c                   clear                   pem01
     c                   read      @wmemp                                 LR

      ** If not EOF, update new fields, write Officer/Owner record

     c                   if        *inLR=*off                                    >---+
     c                   eval      emEMP#=emEMP#@                                    :
     c                   eval      emDIV=emDIV@                                      :
     c                   eval      emDT=emDT@                                        :
     c                   eval      emLIDT=emLIDT@                                    :
     c                   eval      emED01=emED01@                                    :
     c                   eval      emED02=emED02@                                    :
     c                   eval      emED03=emED03@                                    :
     c                   eval      emED04=emED04@                                    :
     c                   eval      emED05=emED05@                                    :
     c                   eval      emED06=emED06@                                    :
     c                   eval      emED07=emED07@                                    :
     c                   eval      emEFD1=emEFD1@                                    :
     c                   eval      emEFD2=emEFD2@                                    :
     c                   eval      emEFD3=emEFD3@                                    :
     c                   eval      emEFD4=emEFD4@                                    :
     c                   eval      emEFD5=emEFD5@                                    :
     c                   eval      emCAD1=emCAD1@                                    :
     c                   eval      emCAD2=emCAD2@                                    :
     c                   eval      emCAD3=emCAD3@                                    :
     c                   eval      emCAD4=emCAD4@                                    :
     c                   eval      emCAD5=emCAD5@                                    :
     c                   eval      emSIC=emSIC@                                      :
     c                   move      emSEMP@       xxF007            7                 :
     c                   eval      emSEMP=xxF007                                     :
                                                                                     :
      **       Create Officer/Owner records if data exists in WMEMP                  :
                                                                                     :
     c                   eval      ooPntr=%addr(emOLN1)                              :
     c                   if        ooOLSN<>*blanks                               >-+ :
     c                   exsr      $MakeWMOOP                                      : :
     c                   endif                                                   <-+ :
     c                   eval      ooPntr=%addr(emOLN2)                              :
     c                   if        ooOLSN<>*blanks                               >-+ :
     c                   exsr      $MakeWMOOP                                      : :
     c                   endif                                                   <-+ :
     c                   eval      ooPntr=%addr(emOLN3)                              :
     c                   if        ooOLSN<>*blanks                               >-+ :
     c                   exsr      $MakeWMOOP                                      : :
     c                   endif                                                   <-+ :
     c                   eval      ooPntr=%addr(emOLN4)                              :
     c                   if        ooOLSN<>*blanks                               >-+ :
     c                   exsr      $MakeWMOOP                                      : :
     c                   endif                                                   <-+ :
     c                   eval      ooPntr=%addr(emOLN5)                              :
     c                   if        ooOLSN<>*blanks                               >-+ :
     c                   exsr      $MakeWMOOP                                      : :
     c                   endif                                                   <-+ :
     c                   eval      ooPntr=%addr(emOLN6)                              :
     c                   if        ooOLSN<>*blanks                               >-+ :
     c                   exsr      $MakeWMOOP                                      : :
     c                   endif                                                   <-+ :
     c                   eval      ooPntr=%addr(emOLN7)                              :
     c                   if        ooOLSN<>*blanks                               >-+ :
     c                   exsr      $MakeWMOOP                                      : :
     c                   endif                                                   <-+ :
                                                                                     :
      **       Set default values in WMEMP and clear Officer/Owner fields            :
                                                                                     :
     c                   eval      emLSID='1'                                        :
     c                   eval      emOFF#='    1'                                    :
     c                   eval      emEXRT='3'                                        :
     c                   eval      emFDNM=emDBA                                      :
     c                   eval      emRSK#=%subst(emCOM3:1:9)                         :
     c                   clear                   emDBA                               :
     c                   clear                   ooBasing                            :
     c                   write     pem01                                             :
     c                   endif                                                   <---+

      *************************************************
      ** Program termination                         **
      *************************************************
     clr                 if        xxWMEMOPN@=*on                                >-+
     clr                 eval      xxFILE='@WMEMP'                                 :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxWMEMOPN=*on                                 >-+
     clr                 eval      xxFILE='WMEMP'                                  :
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
     csr                 eval      xxWMEMOPN@=*off                                   :
     csr                 eval      xxWMEMOPN=*off                                    :
     csr                 eval      xxWMOOOPN=*off                                    :
     csr                 eval      xxLIB='*LIBL'                                     :
     csr                 eval      xxFILE='@WMEMP'                                   :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWMEMOPN@=*on                                  : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='WMEMP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxWMEMOPN=*on                                   : :
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
     csr                 if        xxWMEMOPN=*off or xxWMOOOPN=*off              >-+ :
     c                             or xxWMEMOPN@=*off                              : :
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
     csr                 when      xxFILE='@WMEMP'                               >-+ :
     csr                 close     @wmemp                               91         : :
     csr                 when      xxFILE='WMEMP'                                >-+ :
     csr                 close     wmemp                                91         : :
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
     csr                 eval      ooCO#=emCO#                                     :
     csr                 eval      ooFND=emFND                                     :
     csr                 eval      ooEMP#=emEMP#                                   :
     csr                 eval      ooDIV=emDIV                                     :
     csr                 eval      ooELID=*blanks                                  :
     csr                 eval      ooEXDT=*zero                                    :
     csr                 eval      ooTITL=*blanks                                  :
     csr                 eval      ooDOB=*zeros                                    :
     csr                 eval      ooSEQ#=*zeros                                   :
     csr                 eval      ooSTAT=emSTAT                                   :
     csr                 eval      ooUSER=emUSER                                   :
     csr                 eval      ooPGM=emPGM                                     :
     csr                 eval      ooTIME=emTIME                                   :
     csr                 eval      ooDT=emDT                                       :
     csr                 eval      ooIUSR=emIUSR                                   :
     csr                 eval      ooIPGM=emIPGM                                   :
     csr                 eval      ooITIM=emITIM                                   :
     csr                 eval      ooIDT=emIDT                                     :
     csr                 eval      ooCPFL=*blanks                                  :
     csr                 write     poo01                                           :
     csr                 endsr                                                   <-+

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
     csr                 when      xxFILE='@WMEMP'                               >-+ :
     csr                 open      @wmemp                               91         : :
     csr                 when      xxFILE='WMEMP'                                >-+ :
     csr                 open      wmemp                                91         : :
     csr                 when      xxFILE='WMOOP'                                >-+ :
     csr                 open      wmoop                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 endsr                                                   <---+
