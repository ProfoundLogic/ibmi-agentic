     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
    H/TITLE SR42408 - TX HCN Endosrsment
     *===============================================================*
     * WC420408                                                      *
      *   Single automatically attached endorsement                   *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Create: 11/10/10    C:33    Project: 139WPL                   *
     *===============================================================*
     * REVISIONS:                                                    *
      //
      // 09/20/24 C:79  Project: WC2952
      //                Recompile
      *                                                               *
      * 01/03/17 C:180 Project SY371 scriptura.                       *
      *                Add Scriptura interface.                       *
      *                                                               *
     * 02/03/11  C:65  Project 139WPL Priss #81, 92                  *
     *                -Ensure form prints for policy page run.       *
     *                                                               *
     * 01/12/11  C:33  Project 139WPL Priss #81, 92                  *
     *                -If runnnig a mid-term endorsement, check for  *
     *                  HCN existence on the policy at any point in  *
     *                  time.                                        *
     *                -Use the widest date range for endorsement     *
     *                  effective date.                              *
     *                                                               *
     * 01/10/11  C:65  Project 139WPL Priss #96                      *
     *                 If current system date is prior to policy     *
     *                 effective date then check for HCN setup using *
     *                 policy effective date.                        *
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
     ** Copybook F specs
     F/COPY SCOPYEN01F
     FWDHCL4    if   e           k disk    rename(phc01:phc04)
     FWMHCL     if   e           k disk    rename(phc01:phc00)
     F***  Begin Add  ***  01/12/11  **************************************
     Fwdhcl5    if   e           k disk    rename(phc01:phc05)
     Fwmmup     uf   e             disk    rename(pmu01:pmu0p)
     F***  End   Add  ***  01/12/11  **************************************
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      **** Begin Add **** 01/03/17 *********************************************
      * Entry
     D SR42408         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                1
     D SR42408         PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqmod2                        1
      **** End   Add **** 01/03/17 *********************************************

     ** Copybook D specs
     D/COPY SCOPYEN01D

     D***  Begin Add  ***  01/12/11  **************************************
     D*** Beg Del *** 09/20/24 *****************************************
     D***/copy wPrEmplr
     D*** End Del *** 09/20/24 *****************************************

     DFirstHCNDt       pr                  like(MUEEDT)
     D***  End   Add  ***  01/12/11  **************************************
    
     DPgmExistsInStk   pr                  extPgm('SRCHKPGM')
     D program                       10                 const
     D exists                         1n

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

     DmidTerm          s              1n
     DmidTermPrt       s              1n
     C***  Begin Add  ***  02/03/11  **************************************
     DpolicyPrt        s              1n
     C***  End   Add  ***  02/03/11  **************************************
     DholdCrtFrm       s              1n   inz
     DxxIn             s                   like(HCIN)
     C***  Begin Add  ***  01/03/17  **************************************
     D QQRRN           s              9  0
     C***  End   Add  ***  01/03/17  **************************************
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
      *** Begin Change *** 01/03/17 ********************************************
     D**** xxForm          c                   const('WC420408')
     D QQFORM          S             15A   inz('WC420408  ')
      *** End   Change *** 01/03/17 ********************************************
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    
    
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters
      ** Begin Delete *******   01/03/17 ***************************************
     C***  *entry        plist
     C***                parm                    qqco#
     C***                parm                    qqfnd
     C***                parm                    qqemp#
     C***                parm                    qqdiv
     C***                parm                    qqfyr
     C***                parm                    qqmode
     C***                parm                    qqmod2
    
     ** Additional input program parameters
     C***  addpl         pList
     C***                parm      *blanks       qqprds
     C***                parm      *blanks       qqcall
     C***                parm                    qqco#
     C***                parm                    qqfnd
     C***                parm                    qqemp#
     C***                parm                    qqdiv
     C***                parm                    qqfyr
     *** End   Delete *******   01/03/17 ***************************************
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
     C*
     C     keyWDHCL4     klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELEMP#
     C                   kfld                    ELDIV
     C                   kfld                    ELFYR
     C                   kfld                    RSJRST
     C*
     C     key1WDHCL4    klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELEMP#
     C                   kfld                    ELDIV
     C                   kfld                    ELFYR
     C                   kfld                    RSJRST
     C***  Begin Chg  ***  01/12/11  **************************************
     C**********         kfld                    sysDat
     C                   kfld                    xxIn
     C***  End   Chg  ***  01/12/11  **************************************
     C*
     C     keyWMHCL      klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    RSJRST
     C*
     C     key1WMHCL     klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    RSJRST
     C***  Begin Chg  ***  01/12/11  **************************************
     C**********         kfld                    sysDat
     C                   kfld                    HCIN
     C***  End   Chg  ***  01/12/11  **************************************
    
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

     C                   z-add     5             x                 3 0

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
    
     ** Write the output record
     C***  Begin Add  01/12/11  *******************************************
      *** Begin Delete *** 01/03/17 ********************************************
     C***                select
     C***                when      midTermPrt
     C***  Begin Add  ***  02/03/11  **************************************
     c***                          or policyPrt
     C***  End   Add  ***  02/03/11  **************************************
     C***  End   Add  01/12/11  *******************************************
     C***                write     pfm01
     C***  Begin Add  01/12/11  *******************************************
     C***                endsl
     C***  End   Add  01/12/11  *******************************************
      *** End   Delete *** 01/03/17 ********************************************
     C                   endsr
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     C*
     C***  Begin Add  ***  01/12/11  **************************************
     C                   callp     PgmExistsInStk('W0051R1':midTerm)
     C***  End   Add  ***  01/12/11  **************************************
     C***  Begin Add  ***  01/12/11  **************************************
     C                   callp     PgmExistsInStk('W7051R1':midTermPrt)
     C***  End   Add  ***  01/12/11  **************************************
     C***  Begin Add  ***  02/03/11  **************************************
     C                   callp     PgmExistsInStk('W7413R':policyPrt)
     C***  End   Add  ***  02/03/11  **************************************
      *
     C                   time                    dsdttm
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         sysdat            7 0
      *
     c*** begin add ***  01/10/11  ***********************************************
     C     elky          Chain     pel01
     C***  Begin Chg  ***  01/12/11  **************************************
     c**********         if        sysdat < elpfdt
     c**********         z-add     elpfdt        sysdat
     c**********         endif
     C                   select
     C                   when      midTerm
     C                   eval      xxIn = *hival
     C                   when      sysdat < elpfdt
     C                   eval      xxIn = elpfdt
     C                   other
     C                   eval      xxIn = sysDat
     C                   endsl
     C***  End   Chg  ***  01/12/11  **************************************
     c*** end add *****  01/10/11  ***********************************************
      /Free
       RunningSRtext = *ON;
      /End-Free
     C                   endsr
    
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C*
     C***  Begin Add  ***  01/12/11  **************************************
     C                   select
     C                   when      addMode
     C                   exsr      UpdateInfo
     C                   endsl
     C*
     C                   select
     C                   when      midTermPrt and holdCrtFrm
     C                   eval      xxDiv = GetStRptDv(xxCo#:xxFnd:xxEmp#)
     C                   exsr      BuldIt
     C                   exsr      IncRec
     C                   move      qqemp#        xxemp#
     C                   eval      qqMod2 = holdCrtFrm
     C                   endsl
     C***  End   Add  ***  01/12/11  **************************************
     C*
     C                   endsr
    
     C***  Begin Add  ***  01/12/11  **************************************
     *=========================================================================
     *  Update information according to endorsement specs
     *=========================================================================
     C     UpdateInfo    begsr
     C*
     C     muRRN4        chain     pmu0p
     C                   if        %found
     C                   eval      MUDIV  = GetStRptDv(MUCO#:MUFND:MUEMP#)
     C                   eval      MUEEDT = FirstHCNDt
     C                   eval      MUEND# = *zero
     C                   update    pmu0p
     C                   endif
     C*
     C                   endsr
     C***  End   Add  ***  01/12/11  **************************************
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr

     C                   eval      crtFrm = *on
    
     C     rsky          chain     prs01                              60
    
     ** Found a division with a matching jurisdiction state
    
     C                   if        *in60
     C                              or RSJRST <> ENST
     C                   eval      crtFrm = *off
     C                   endif

     * Check that employer division is not cancelled

     c                   exsr      ck_emp_cx

     c                   if        emp_cx = 'Y'
     C                   eval      crtFrm = *off
     C                   endif

     C* Check for HCN Program assigned to employer
     C     key1WDHCL4    setgt     WDHCL4
     C     keyWDHCL4     readpe    WDHCL4
     C***  Begin Add  ***  01/12/11  **************************************
     C                   select
     C                   when      midTerm and HCOUT = HCIN
     C                   dou       %eof or HCOUT <> HCIN
     C     keyWDHCL4     readpe    WDHCL4
     C                   enddo
     C                   endsl
     C***  End   Add  ***  01/12/11  **************************************
     C                   if        %eof
     C                              or (HCOUT <> *zero
     C***  Begin Chg  ***  01/12/11  **************************************
     C**********                        and HCOUT < sysDat)
     C                                  and HCOUT < xxIn)
     C***  End   Chg  ***  01/12/11  **************************************
     C                   eval      crtFrm = *off
     C                   endif

     C* Check for HCN Program at group to have this endorsement assigned
     C     key1WMHCL     setgt     WMHCL
     C     keyWMHCL      readpe    WMHCL
     C                   if        %eof
     C                              or (HCEXDT <> *zero
     C***  Begin Chg  ***  01/12/11  **************************************
     C**********                        and HCEXDT < sysDat)
     C                                  and HCEXDT < HCIN)
     C***  End   Chg  ***  01/12/11  **************************************
     C                              or HCFORM <> xxForm
     C                   eval      crtFrm = *off
     C                   endif
     C*
     C***  Begin Add  ***  01/12/11  **************************************
     C                   if        midTermPrt and crtFrm = *on
     C                   eval      holdCrtFrm = crtFrm
     C                   eval      crtFrm = *off
     C                   endif
     C***  End   Add  ***  01/12/11  **************************************
    
     C                   endsr
    
     P***  Begin Add  ***  01/12/11  **************************************
    P*=========================================================================
    P* Find the first HCN program date for the policy
    P*=========================================================================
     PFirstHCNDt       b
     DFirstHCNDt       pi                  like(MUEEDT)

     D holdHCIN        s                   like(HCIN) inz

     C     keyWDHCL5     klist
     C                   kfld                    MUCO#
     C                   kfld                    MUFND
     C                   kfld                    MUEMP#
     C                   kfld                    MUFYR
    C*
     C                   clear                   holdHCIN
    C*
     C     keyWDHCL5     setll     phc05
     C                   dou       %eof
     C     keyWDHCL5     reade     phc05
     C                   if        not %eof
     C                   select
     C                   when      holdHCIN > HCIN
     C                   eval      holdHCIN = HCIN
     C                   when      holdHCIN = *zero
     C                   eval      holdHCIN = HCIN
     C                   endsl
     C                   endif
     C                   enddo
     C*
     C                   return    holdHCIN
     C*
     PFirstHCNDt       e
     P***  End   Add  ***  01/12/11  **************************************
