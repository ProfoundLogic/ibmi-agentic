     H*  (C) Copyright Progressive Data Solutions, Inc., 1998
     H/TITLE CVTEMEMP: V3 Conversion for File EMEMP
     f@ememp    if   f  181        disk    usropn                                Employer Master
     fememp     o    e             disk    usropn                                Employer Master

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
     d  xxEMEMOPN@                    1a   inz('0')                                @EMEMP open flag
     d  xxEMEMOPN                     1a   inz('0')                                EMEMP open flag
     d  xxLIB                        10a                                           File library
     d  xxFILE                       10a                                           File name
     d  xxCMD                       128a                                           System command
     d  xxLENGTH                     15p 5                                         Command length

      ********************************************************
      ** Old Employer Master File                           **
      ********************************************************
     i@ememp    ns
     i                             P    1    2 0emCO#                            COMPANY NUMBER
     i                             P    3    4 0emFND                            FUND NUMBER
     i                             P    5    7 0emEMP#@                          EMPLOYER NUMBER
     i                                  8   47  emNAME                           NAME
     i                                 48   87  emMAD1                           MAILING ADDRESS
     i                                 88  112  emMCTY                           MAILING CITY
     i                                113  114  emMST                            MAILING STATE
     i                             P  115  119 0emMZIP                           MAILING ZIP COD
     i                                120  125 0emEFD1@                          EFFECTIVE DATE
     i                             P  126  129 0emSEMP@                          STATE EMPLOYER
     i                                130  131  emCNTY                           COUNTY CODE
     i                                132  132  emMOD                            CALC EXP MOD Y/
     i                                133  133  emSTAT                           STATUS
     i                                134  143  emUSER                           USER
     i                                144  153  emPGM                            PROGRAM
     i                                154  159 0emTIME                           TIME
     i                                160  165 0emDT@                            DATE MAINTAINED
     i                                166  175  emIUSR                           INITIAL USER
     i                                176  181 0emIDT@                           INITIAL ENTRY D

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

      ** Read next old Employer Master record

     c                   clear                   pem01
     c                   read      @ememp                                 LR

      ** If not EOF, write new Employer Master record

     c                   if        *inLR=*off                                    >-+
     c                   eval      emEMP#=emEMP#@                                  :
     c                   eval      emEFD1=emEFD1@                                  :
     c                   eval      emDT=emDT@                                      :
     c                   eval      emIDT=emIDT@                                    :
     c                   move      emSEMP@       xxF007            7               :
     c                   eval      emSEMP=xxF007                                   :
     c                   write     pem01                                           :
     c                   endif                                                   <-+

      *************************************************
      ** Program termination                         **
      *************************************************
     clr                 if        xxEMEMOPN@=*on                                >-+
     clr                 eval      xxFILE='@EMEMP'                                 :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+
     clr                 if        xxEMEMOPN=*on                                 >-+
     clr                 eval      xxFILE='EMEMP'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+

      *************************************************
      ** Program initialization                      **
      *************************************************
     csr   *inzsr        begsr                                                   >---+
     csr                 eval      xxEMEMOPN=*off                                    :
     csr                 eval      xxLIB='*LIBL'                                     :
     csr                 eval      xxFILE='@EMEMP'                                   :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxEMEMOPN@=*on                                  : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='EMEMP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxEMEMOPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 if        xxEMEMOPN@=*off or xxEMEMOPN=*off             >-+ :
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
     csr                 when      xxFILE='@EMEMP'                               >-+ :
     csr                 close     @ememp                               91         : :
     csr                 when      xxFILE='EMEMP'                                >-+ :
     csr                 close     ememp                                91         : :
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
     csr                 when      xxFILE='@EMEMP'                               >-+ :
     csr                 open      @ememp                               91         : :
     csr                 when      xxFILE='EMEMP'                                >-+ :
     csr                 open      ememp                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 endsr                                                   <---+
