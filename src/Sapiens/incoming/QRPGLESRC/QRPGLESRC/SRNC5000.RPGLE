     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     *
    H/TITLE SRNC5000 - Form NC5000 Print/Attach program
     *===============================================================*
     * NC5000                                                        *
      *   Single automatically attached endorsement                   *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 01/12/11  C:190 Project 132WOY                                *
     *                 Created.                                      *
     *                                                               *
     *===============================================================*
     * Notes:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     *                                                               *
     *  QQMODE = 1 - Called to determine if form should be attached. *
     *  QQMODE = 2 - Called to print endorsement.                    *
     *  QQMODE = 3 - Called for additional input.                    *
     *  QQMODE = 4 - Called for mid-term processing.                 *
     *===============================================================*
    
     fwmepl     if   e           k disk
     fwmprl1    if   e           k disk
     ** Copybook F specs
     F/COPY SCOPYEN01F
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     D/COPY SCOPYEN01D
    

      * employer effective dates
     d edds            ds
     d  emefd1
     d  emefd2
     d  emefd3
     d  emefd4
     d  emefd5
    d ed                                  OVERLAY(edds)
     d                                     LIKE(emefd1) DIM(5)

      * employer cancel dates
     d cdds            ds
     d  emcad1
     d  emcad2
     d  emcad3
     d  emcad4
     d  emcad5
    d cd                                  OVERLAY(cdds)
     d                                     LIKE(emcad1) DIM(5)

     D                 ds
     D xxname                       100
     D  fmemnm                             overlay(xxname)
     D  fmc215                             overlay(xxname:41)
     D  fmc441                             overlay(xxname:61)
     D                 ds
     D xxfnam                       100
     D  fmfenm                             overlay(xxfnam)
     D  fmc216                             overlay(xxfnam:41)
     D  fmc442                             overlay(xxfnam:61)

      * Data base fields.
     d dbds            ds
     d  fm2001
     d  fm2002
     d  fm2003
     d  fm2004
     d  fm2005
    d db                                  OVERLAY(dbds)
     d                                     LIKE(fm2001) DIM(5)
      *

     d j               s              2  0
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D xxform          c                   const('NC5000    ')
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    
    
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
    
    
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters
     C     *entry        plist
     C                   parm                    qqco#
     C                   parm                    qqfnd
     C                   parm                    qqemp#
     C                   parm                    qqdiv
     C                   parm                    qqfyr
     C                   parm                    qqmode
     C                   parm                    qqrrn
     C                   parm                    qqmod2
    
     ** Additional input program parameters
     C     addpl         pList
     C                   parm      *blanks       qqprds
     C                   parm      *blanks       qqcall
     C                   parm                    qqco#
     C                   parm                    qqfnd
     C                   parm                    qqemp#
     C                   parm                    qqdiv
     C                   parm                    qqfyr
    
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    
     * Employer Master
    
     C     emky1         klist
     C                   KFld                    elco#
     C                   KFld                    elfnd
     C                   KFld                    elemp#
     C                   KFld                    eldiv
    C*
     C     epky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    eldiv
     C                   kfld                    elfyr
    C*
     C     prky          klist
     C                   kfld                    xxco#
     C                   kfld                    elrtcd
     C                   kfld                    epclas
     C                   kfld                    elpfdt
    C*
     C     prky1         klist
     C                   kfld                    xxco#
     C                   kfld                    elrtcd
     C                   kfld                    epclas
    C*
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
    
     *=========================================================================
     *  CK_EMP_CX - Check if employer division is cancelled
     *=========================================================================
     c     ck_emp_cx     begsr

     c                   move      'Y'           emp_cx            1
     c     emky1         chain     pem01

     c                   if        %found

      *  Get Employer Effective/Cancellation date

     c                   z-add     5             x                 3 0

     c                   dow       (ed(x) = 0) and (x > 1)
     c                   sub       1             x
     c                   enddo

     c                   if        (cd(x) = 0) or (cd(x) > sysdat)
     c                   eval      emp_cx = 'N'
     c                   endif

     c                   endif

     c                   endsr

    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
      *
     c                   move      qqco#         #5co#
     c                   move      qqfnd         #5fnd
     c                   move      qqemp#        #5emp#
     c                   move      qqdiv         #5div
     c                   move      qqfyr         #5fyr

     c                   call      'SRADD1'
     c                   parm                    #5co#             3 0
     c                   parm                    #5fnd             3 0
     c                   parm                    #5emp#            9 0
     c                   parm                    #5div             5 0
     c                   parm                    #5fyr             3 0
     c     xxname        parm                    #5name          100
     c     xxfnam        parm                    #5fnam          100
     c                   parm                    #5ead1           40
     c                   parm                    #5ead2           40
     c                   parm                    #5ecsz           40
     C                   parm                    #5cty            25
     C                   parm                    #5st              2
     C                   parm                    #5zip             9 0
     C                   parm                    #5fein            9 0
     C                   parm                    #5eadr           90

     ** Write the output record

     c                   eval      j = 1
     c                   clear                   db

     c                   if        #5name <> *blank
     c                   eval      db(j) = %trimr(#5name)
     c                   eval      j = j + 1
     c                   endif
     c*
     c                   if        #5fnam <> *blank
     c                   eval      db(j) = %trimr(#5fnam)
     c                   eval      j = j + 1
     c                   endif

     c                   if        #5ead1 <> *blank
     c                   eval      db(j) = %trimr(#5ead1)
     c                   eval      j = j + 1
     c                   endif

     c                   if        #5ead2 <> *blank
     c                   eval      db(j) = %trimr(#5ead2)
     c                   eval      j = j + 1
     c                   endif

     c                   if        #5ecsz <> *blank
     c                   eval      db(j) = %trimr(#5ecsz)
     c                   eval      j = j + 1
     c                   endif
     c*
     C                   write     pfm01
     C                   endsr
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
      *
     C                   time                    dsdttm
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         sysdat            7 0
      *
     C                   endsr
    
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
    
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
    
     C     rsky          chain     prs01                              60
    
     ** Found a division with a matching jurisdiction state
    
     C                   if        %found
     C                   if        (enst = *blank) or (rsjrst = enst)
      *
     C     epky          setll     pep01
     C     epky          reade     pep01                                  60
      *
     C     *in60         doweq     *off
      *
     C     epstat        ifne      'I'
     C     prky          setgt     ppr01
     C     prky1         readpe    ppr01                                  60
      *
     C     *in60         ifeq      *off
     C     prccfl        andeq     'Y'

     * Check that employer division is not cancelled

     c                   exsr      ck_emp_cx

     c                   if        emp_cx = 'N'
     C                   move      *on           crtfrm
     C                   endif                                                  emp_cx = 'N'
    
     C                   endif                                                  prccfl = 'Y'
     C                   endif                                                  epstat <>'I'

     C     epky          reade     pep01                                  60
     C                   enddo
     C                   endif                                                  *in60 = *off
     C                   endif                                                  %found
    
     C                   endsr                                                  Valid
    
