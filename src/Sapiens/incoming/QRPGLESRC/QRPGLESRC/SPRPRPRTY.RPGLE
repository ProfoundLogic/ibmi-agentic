     P*=====================================================================
     P* AddProperty - Add a system property value
     P*   Parm 1 - property key
     P*   Parm 2 - property value
     P*   Parm 3 - overwrite current value, if exists {optional}
     P*   Returns = *ON-Added (or updated); *OFF-not added
     P*=====================================================================
     DAddProperty      pr             1n
     D inKey                         25
     D inProperty                   256
     D inOverwrite                    1n   options(*nopass)

     P*=====================================================================
     P* IAccessURL - Retrieve the URL address for iAccess
     P*   Returns = Character string with address
     P*=====================================================================
     DIAccessURL       pr           256

     P*=====================================================================
     P* SysProperty - Retrieve the system property values
     P*   Parm 1 - property key
     P*   Parm 2 - failure notification {optional}
     P*   Returns = property value
     P*=====================================================================
     DSysProperty      pr           256
     D inKey                         25    const
     D inFailNotify                   1n   const options(*nopass)
