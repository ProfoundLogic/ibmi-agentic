     Hcopyright('(c) Copyright Fiserv, Inc., 2007')
     H*****************************************************************
     H/TITLE FIXWDXSP_1 - Split records from Quote & Live
     H*****************************************************************
     H*  Created: 03/14/07    C:33    Seq#: WA045/54881               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FWMEML     if   e           k disk    usropn rename(pem01:pem00)
     FWMEMLQ    if   e           k disk    usropn rename(pem01:pem0q)
     FWDXSL     uf   e           k disk    usropn rename(pxs01:pxs00)
     FWDXSLQ    o    e             disk    usropn rename(pxs01:pxs0q)
      *
     Fqsysprt   o    f  132        printer oflind(*inoa) usropn
      *
     D count           s              5  0
     D countWDXSP      s                   like(count) inz
     D empCount        s                   like(count) inz
     D printOnly       s              1n
     D saveCo#         s                   like(XSCO#)
     D saveFnd         s                   like(XSFND)
     D saveEmp#        s                   like(XSEMP#)
     D updateFlag      s              1
     D updateOnly      s              1n
     D @@LiveLib       s             10
     D @@QteLib        s             10
     D @@UpdateFlag    s              1

     D psds          esds                  extname(szq1p)

     DDltOvrs          pr
     D inFile                        10    const
     D inEnv                         10    const
     DDltOvrCmd        pr           255
     D inFile                        10    const
     D inEnv                         10    const
     DOvrCmd           pr           255
     D inFile                        10    const
     D inEnv                         10    const
     DOverrides        pr
     D inFile                        10    const
     D inEnv                         10    const

     D/copy sPrRunCmd
     D/copy sPrDate

     C     keyWMEML      klist
     C                   kfld                    XSCO#
     C                   kfld                    XSFND
     C                   kfld                    XSEMP#
     C*
     C     *entry        plist
     C                   parm                    @@LiveLib
     C                   parm                    @@QteLib
     C                   parm                    @@UpdateFlag
     C*
     C                   if        %parms >= 3
     C                   eval      updateFlag = @@UpdateFlag
     C                   endif
     C*
     C                   select
     C                   when      updateFlag = 'B'
     C                   eval      updateOnly = *off
     C                   eval      printOnly  = *off
     C                   when      updateFlag = 'P'
     C                   eval      updateOnly = *off
     C                   eval      printOnly  = *on
     C                   when      updateFlag = 'U'
     C                   eval      updateOnly = *on
     C                   eval      printOnly  = *off
     C                   other
     C                   eval      updateOnly = *off
     C                   eval      printOnly  = *on
     C                   endsl
     C*
     C                   if        not updateOnly
     C                   open      qsysprt
     C                   eval      *inoa = *on
     C                   endif
     C*
     C                   exsr      OvrDbf
     C*
     C     *loval        setll     wdxsl
     C                   dou       %eof(wdxsl)
     C                   read      wdxsl
     C                   if        not %eof
     C                   exsr      CheckRcd
     C                   endif
     C                   enddo
     C*
     C                   if        not updateOnly
     C                   except    empTotal
     C                   except    totals
     C                   endif
     C*
     C                   exsr      DltOvr
     C*
     C                   MOVE      *ON           *INLR
     C*
     C*====================================================================
     C* Check the records
     C*====================================================================
     C     CheckRcd      begsr
     C*
     C     keyWMEML      chain     WMEML
     C     keyWMEML      chain     WMEMLQ
     C*
     C                   select
     C                   when      %found(WMEML) and %found(WMEMLQ)
     C                   exsr      WriteWDXSLQ
     C                   exsr      PrintRcds
     C                   when      %found(WMEMLQ)
     C                   exsr      WriteWDXSLQ
     C                   exsr      DeleteWDXSL
     C                   when      %found(WMEML)
     C                   other
     C                   exsr      DeleteWDXSL
     C                   endsl
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Delete the record from WDXSL
     C*====================================================================
     C     DeleteWDXSL   begsr
     C*
     C                   if        not printOnly
     C                   delete    WDXSL
     C                   endif
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Delete overrides
     C*====================================================================
     C     DltOvr        begsr
     C*
     C                   close     WDXSL
     C                   close     WDXSLQ
     C                   callp     DltOvrs('WDXSL' :'LIVE')
     C                   callp     DltOvrs('WDXSLQ':'QTE' )
     C*
     C                   close     WMEML
     C                   close     WMEMLQ
     C                   callp     DltOvrs('WMEML' :'LIVE')
     C                   callp     DltOvrs('WMEMLQ':'QTE' )
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Override files
     C*====================================================================
     C     OvrDbf        begsr
     C*
     C                   callp     Overrides('WDXSL' :'LIVE')
     C                   callp     Overrides('WDXSLQ':'QTE' )
     C                   open      WDXSL
     C                   open      WDXSLQ
     C*
     C                   callp     Overrides('WMEML' :'LIVE')
     C                   callp     Overrides('WMEMLQ':'QTE' )
     C                   open      WMEML
     C                   open      WMEMLQ
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Print information on report
     C*====================================================================
     C     PrintRcds     begsr
     C*
     C                   if        *inoa and not updateOnly
     C                   except    head
     C                   eval      *inoa = *off
     C                   endif
     C*
     C                   if        XSCO# <> saveCo#
     C                              or XSFND  <> saveFnd
     C                              or XSEMP# <> saveEmp#
     C                   if        not updateOnly
     C                              and saveCo# <> *zero
     C                   except    empTotal
     C                   endif
     C                   eval      empCount = *zero
     C                   eval      saveCo#  = XSCO#
     C                   eval      saveFnd  = XSFND
     C                   eval      saveEmp# = XSEMP#
     C                   endif
     C*
     C                   eval      empCount = empCount + 1
     C                   if        not updateOnly
     C                   except    detail
     C                   endif
     C                   eval      countWDXSP = countWDXSP + 1
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Write a copy of the record to WDXSLQ
     C*====================================================================
     C     WriteWDXSLQ   begsr
     C*
     C                   eval      XSPGM  = q1pgm
     C                   eval      XSUSER = q1user
     C                   eval      XSTIME = TimeTo6(SystemTime)
     C                   eval      XSDT   = DateTo7(SystemDate)
     C*
     C                   if        not printOnly
     C                   write     pxs0q
     C                   endif
     C*
     C                   endsr
    C*====================================================================
     Oqsysprt   e            head             04
     O                                          +40 'Records Written'
     O          e            head        2  2
     O                                          + 5 'Live Library:'
     O                       @@LiveLib          + 1
     O                                          + 3 'Quote Library:'
     O                       @@QteLib           + 1
     O          e            head           2
     O                                          + 2 'Co#'
     O                                          + 2 'Grp'
     O                                          + 2 ' Employer'
     O                                          + 2 'Fyr'
     O                                          + 2 'St'
     O          e            detail         1
     O                       XSCO#         z    + 2
     O                       XSFND         z    + 2
     O                       XSEMP#        z    + 2
     O                       XSFYR         z    + 2
     O                       XSST               + 2
     O          e            empTotal       2
     O                                          + 4 'Count:'
     O                       empCount      z    + 2
     O          e            totals         2
     O                                          + 4 'Total:'
     O                       countWDXSP    z    + 2





     P*====================================================================
     P* Create Delete Override command
     P*====================================================================
     PDltOvrCmd        b
     DDltOvrCmd        pi           255
     D inFile                        10    const
     D inEnv                         10    const

     D cmd             s                   like(OvrCmd)

     C                   eval      cmd = 'DLTOVR FILE(' + %trim(inFile)
     C                                 + ') '
     C                                 + 'LVL(*JOB)'

     C                   return    cmd

     PDltOvrCmd        e





     P*====================================================================
     P* Perform Override Deletions
     P*====================================================================
     PDltOvrs          b
     DDltOvrs          pi
     D inFile                        10    const
     D inEnv                         10    const

     D cmd             s            255

     C                   eval      cmd = DltOvrCmd(inFile:inEnv)
     C                   callp     RunCmd(cmd:%len(%trim(cmd)))

     PDltOvrs          e





     P*====================================================================
     P* Perform Overrides
     P*====================================================================
     POverrides        b
     DOverrides        pi
     D inFile                        10    const
     D inEnv                         10    const

     D cmd             s            255

     C                   eval      cmd = OvrCmd(inFile:inEnv)
     C                   callp     RunCmd(cmd:%len(%trim(cmd)))

     POverrides        e





     P*====================================================================
     P* Create Override command
     P*====================================================================
     POvrCmd           b
     DOvrCmd           pi           255
     D inFile                        10    const
     D inEnv                         10    const

     D cmd             s                   like(OvrCmd)

     C                   eval      cmd = 'OVRDBF FILE(' + %trim(inFile)
     C                                 + ') TOFILE('
     C                   select
     C                   when      inEnv = 'LIVE'
     C                   eval      cmd = %trim(cmd)
     C                                 + %trim(@@LiveLib)
     C                   when      inEnv = 'QTE'
     C                   eval      cmd = %trim(cmd)
     C                                 + %trim(@@QteLib)
     C                   endsl
     C                   eval      cmd = %trim(cmd)
     C                                 + '/' + %trim(inFile) + ') '
     C                                 + 'OVRSCOPE(*JOB)'

     C                   return    cmd

     POvrCmd           e





