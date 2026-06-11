     H*  (C) Copyright Progressive Data Solutions, Inc., 1999
     H/TITLE CVTIMITP: V3 Conversion for File IMITP
     f@imitp    if   f  129        disk    usropn                                old Index Type File
     fimitp     o    e             disk    usropn                                new Index Type File

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
     d  xxIMITOPN@                    1a   inz('0')                                @IMITP open flag
     d  xxIMITOPN                     1a   inz('0')                                IMITP open flag
     d  xxLIB                        10a                                           File library
     d  xxFILE                       10a                                           File name
     d  xxCMD                       128a                                           System command
     d  xxLENGTH                     15p 5                                         Command length

      *************************************************
      ** Old WACTP file layout                       **
      *************************************************
     i@imitp    ns
     i                             a    1   20  itdesc
     i                             a   21   30  itpnam
     i                             a   31   31  itstat
     i                             a   32   41  ituser
     i                             a   42   51  itpgm
     i                             s   52   57 0ittime
     i                             s   58   63 0itdt@
     i                             a   64   73  itiusr
     i                             a   74   83  itvpgm
     i                             a   84   93  itdpgm

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

      ** Read next old Index Type record

     c                   clear                   pit01
     c                   read      @imitp                                 LR

      ** If not EOF, write new Index Type record

     c                   if        *inLR=*off
     c                   Eval      itdt = itdt@
     c                   write     pit01
     c                   endif

      *************************************************
      ** Program termination                         **
      *************************************************
     clr                 if        xxIMITOPN@=*on
     clr                 eval      xxFILE='@IMITP'
     clr                 exsr      $Close
     clr                 endif
     clr                 if        xxIMITOPN=*on
     clr                 eval      xxFILE='IMITP'
     clr                 exsr      $Close
     clr                 endif

      *************************************************
      ** Program initialization                      **
      *************************************************
     csr   *inzsr        begsr
     csr                 eval      xxIMITOPN=*off
     csr                 eval      xxLIB='*LIBL'
     csr                 eval      xxFILE='@IMITP'
     csr                 exsr      $Check
     csr                 if        *in91=*off
     csr                 eval      xxIMITOPN@=*on
     csr                 exsr      $Open
     csr                 endif
     csr                 eval      xxLIB=pmCLTLIB
     csr                 eval      xxFILE='IMITP'
     csr                 exsr      $Check
     csr                 if        *in91=*on
     csr                 eval      xxLIB=pmBASELIB
     csr                 exsr      $Check
     csr                 endif
     csr                 if        *in91=*off
     csr                 eval      xxIMITOPN=*on
     csr                 exsr      $Open
     csr                 endif
     csr                 if        xxIMITOPN@=*off or xxIMITOPN=*off
     csr                 eval      *inLR=*on
     csr                 endif
     csr                 endsr

      *************************************************
      ** Check for file                              **
      *************************************************
     csr   $Check        begsr
     csr                 if        %subst(xxFILE:1:1)='@'
     csr                 eval      xxCMD=cnCHKOBJ+%trim(xxLIB)+'/'+
     c                             %trim(%subst(xxFILE:2))+cnOBJTYPE
     csr                 else
     csr                 eval      xxCMD=cnCHKOBJ+%trim(xxLIB)+'/'+
     c                             %trim(xxFILE)+cnOBJTYPE
     csr                 endif
     csr                 eval      xxLENGTH=128
     csr                 call      'QCMDEXC'                            91
     csr                 parm                    xxCMD
     csr                 parm                    xxLENGTH
     csr                 endsr

      *************************************************
      ** Close a file                                **
      *************************************************
     csr   $Close        begsr
     csr                 select
     csr                 when      xxFILE='@IMITP'
     csr                 close     @imitp                               91
     csr                 when      xxFILE='IMITP'
     csr                 close     imitp                                91
     csr                 endsl
     csr                 eval      xxCMD=cnDLTOVR+%trim(xxFILE)+')'
     csr                 eval      xxLENGTH=128
     csr                 call      'QCMDEXC'                            91
     csr                 parm                    xxCMD
     csr                 parm                    xxLENGTH
     csr                 endsr

      *************************************************
      ** Open a file                                 **
      *************************************************
     csr   $Open         begsr
     csr                 if        %subst(xxFILE:1:1)='@'
     csr                 eval      xxCMD=cnOVRDBF+%trim(xxFILE)+cnTOFILE+
     c                             %trim(xxLIB)+'/'+%trim(%subst(xxFILE:2))+')'
     csr                 else
     csr                 eval      xxCMD=cnOVRDBF+%trim(xxFILE)+cnTOFILE+
     c                             %trim(xxLIB)+'/'+%trim(xxFILE)+')'
     csr                 endif
     csr                 eval      xxLENGTH=128
     csr                 call      'QCMDEXC'                            91
     csr                 parm                    xxCMD
     csr                 parm                    xxLENGTH
     csr                 select
     csr                 when      xxFILE='@IMITP'
     csr                 open      @imitp                               91
     csr                 when      xxFILE='IMITP'
     csr                 open      imitp                                91
     csr                 endsl
     csr                 endsr
