     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')

    H/title Generate Key Code
    H*********************************************************************
    H*  DESCRIPTION:
    H*
    H*  Some programs are secured with a Key Code such that they may
    H*  only be run by StoneRiver Employees.
    H*
    H*  The Key Codes are based upon system time and are only valid for
    H*  a few minutes from the time entered.  (Time defaults to current
    H*  system time).  Each Key Code can only be used once.
    H*
    H*  This program generates valid Key Codes.
      *
      *  Note: CIPHER procedure in SA720R and SS030R must be the same.
    H*
    H*********************************************************************
    H* REVSIONS:
    H*
    H* 10/19/11 C:061 Project 139IQJ
    H*                Created.
    H*
    H*********************************************************************

     Fss030f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    
     D/COPY SCOPYPR01D
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------

     d datetime        s               z

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

     D/COPY sprsecure

     d*entry
     d SS030R          pr
     d SS030R          pi


    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C/COPY SCOPYPR01C
    C*
    C*====================================================================
    C* DSCAPI - Call API for getting descriptions (use 91 on error)
    C*====================================================================
    C*
     C     DSCAPI        begsr

     C                   endsr
    
    
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
    C*
     C     ERRCK1        begsr
      *
      * Default to current Hours and Minutes
      *
     C  n90              if        Not On_PDSDEV()
     c                   eval      *in90 = *on
     c                   eval      *in73 = *on
     c                   exfmt     scr01
     c                   eval      *inlr = *on
     c                   return
     C                   endif
      *
      * Default to current Hours and Minutes
      *
     C  n90              if        schhmm = *blanks
     C                   eval      schhmm = %char(%time():*hms0)
     C                   endif
     C*
     c                   movel     schhmm        xxhh              2 0
     c                   move      schhmm        xxmm              2 0
      *
      * Hours must be valid
      *
     c  n90              if        (xxhh > 23)
     c                   eval      *in90 = *on
     c                   eval      *in71 = *on
     c                   endif
      *
      * Minutes must be valid
      *
     c  n90              if        (xxmm > 59)
     c                   eval      *in90 = *on
     c                   eval      *in72 = *on
     c                   endif

     C                   endsr
    C*
    C*
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
    C*
     C     OTHFKS        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
    C*
     C     PGMEND        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PGMSTR - Used for any start of program processing (one-time)
    C*====================================================================
    C*
     C     PGMSTR        begsr

     * Default to current hh/mm
    
     C                   eval      schhmm = %char(%time():*hms0)
    
     C                   endsr
    C*
    C*====================================================================
    C* PREDSP - Pre-processing before displaying (every time)
    C*====================================================================
    C*
     C     PREDSP        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PRSCTL - Process control after no errors found (Call Submitter)
    C*====================================================================
    C*
     C     PRSCTL        begsr
      /free

           // Generate new KeyCode (If at PDSDEV)

               datetime = %timestamp((%char(%date():*iso0) + schhmm +
                               '00000000'): *iso0);
               sccode = New_KeyCode(datetime);
               f3reqd = *on;

      /end-free
     C                   endsr


