     C*====================================================================
     C*  Created: 05/20/09   C:33   Project: WC1770
     C*====================================================================
     C*  Revisions:
     C*
     C*====================================================================
     C*
     C     *entry        plist
     C                   parm                    @@rrn             9 0
     C                   parm                    @@mode            1
     C                   parm                    @@from            2
     C                   parm                    @@to              2
     C* @@mode
     C*  'D' = Delete the information from the @@to file
     C*  'R' = Return RRN of the @@to file
     C*  'U' = Update the information in the @@to file from the @@from file
     C* @@from/@@to
     C*  'SY' = system file
     C*  'MC' = system file
     C*
     C                   clear                   sysRRN
     C                   clear                   mcRRN
     C*
     C                   select
     C                   when      @@from = 'SY'
     C     @@rrn         chain(n)  syspf
     C     key           chain     syslf
     C     key           chain     mclf
     C                   when      @@from = 'MC'
     C     @@rrn         chain(n)  mcpf
     C     key           chain     syslf
     C     key           chain     mclf
     C                   endsl
     C*
     C                   select
     C                   when      @@mode = 'D'
     C                   exsr      DeleteRcd
     C                   when      @@mode = 'R'
     C                   exsr      ReturnRRN
     C                   when      @@mode = 'U'
     C                   exsr      UpdateFile
     C                   endsl
     C*
     C                   eval      *inlr = *on
     C*
     C*====================================================================
     C* Delete record
     C*====================================================================
     C     DeleteRcd     begsr
     C*
     C                   exsr      GetRcds
     C*
     C                   if        fromFound and toFound
     C                   select
     C                   when      @@to = 'SY'
     C                   delete    syspf
     C                   when      @@to = 'MC'
     C                   delete    mcpf
     C                   endsl
     C                   endif
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Get records
     C*====================================================================
     C     GetRcds       begsr
     C*
     C* Determine if the @@to record already exists
     C                   select
     C                   when      @@to = 'SY'
     C     sysRRN        chain     syspf
     C                   eval      toFound = %found
     C                   when      @@to = 'MC'
     C     mcRRN         chain     mcpf
     C                   eval      toFound = %found
     C                   endsl
     C*
     C* Get the data from the @@from record
     C                   select
     C                   when      @@from = 'SY'
     C     sysRRN        chain     syspf
     C                   eval      fromFound = %found
     C                   when      @@from = 'MC'
     C     mcRRN         chain     mcpf
     C                   eval      fromFound = %found
     C                   endsl
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Return RRNs
     C*====================================================================
     C     ReturnRRN     begsr
     C*
     C                   select
     C                   when      @@to = 'SY'
     C                   eval      @@rrn = sysRRN
     C                   when      @@to = 'MC'
     C                   eval      @@rrn = mcRRN
     C                   endsl
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Update files
     C*====================================================================
     C     UpdateFile    begsr
     C*
     C                   exsr      GetRcds
     C*
     C                   if        fromFound
     C                   select
     C                   when      toFound
     C                   exsr      UpdateRcd
     C                   other
     C                   exsr      WriteRcd
     C                   endsl
     C                   endif
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Update record
     C*====================================================================
     C     UpdateRcd     begsr
     C*
     C                   select
     C                   when      @@to = 'SY'
     C                   update    syspf
     C                   when      @@to = 'MC'
     C                   update    mcpf
     C                   endsl
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Write record
     C*====================================================================
     C     WriteRcd      begsr
     C*
     C                   select
     C                   when      @@to = 'SY'
     C                   write     syspf
     C                   when      @@to = 'MC'
     C                   write     mcpf
     C                   endsl
     C*
     C                   endsr
