     * (C) Copyright FISERV Insurance Solutions, Inc., 2006
     *
    H/TITLE SRPP1A
     *===============================================================*
     * NJ Form PP1A                                                  *
     * Notice of Election - Propietors and Partners                  *
     * Workers Compensation and Employers' Liability Insurance       *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     *===============================================================*
     * REVISIONS:
    H*                                                               *
    H* 03/09/06 C:33  Project: 135WFZ                                *
    H*                Create PP1A as PRTF.                           *
     *
     * 02/06/06  C:61  PROJECT 135WFZ
     *                 Created.
     *
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
    
     * Copybook F specs
     F/COPY SCOPYEN01F
    
      ** Owner / Officer
     Fwmool9    if   e           k disk
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     * Copybook D specs
     D/COPY SCOPYEN01D


      * output employer name
     d enamds          ds
     d  FMNM11
     d  FMNM12

      * output owner/officer names
     d namds           ds
     d  FMNM01
     d  FMNM02
     d  FMNM03
     d  FMNM04
     d  FMNM05
     d  FMNM06
     d  FMNM07
     d  FMNM08
     d  FMNM09
     d  FMNM10
    d nam                                 OVERLAY(namds)
     d                                     LIKE(FMNM01) DIM(10)

      * Include/Exclude owner/officer names
     d InExDS          ds
     d  FMC101
     d  FMC102
     d  FMC103
     d  FMC104
     d  FMC105
     d  FMC106
     d  FMC107
     d  FMC108
     d  FMC109
     d  FMC110
    d InEx                                OVERLAY(InExDS)
     d                                     LIKE(FMC101) DIM(10)

      * counters
     d x1              s              3  0
    

     *=========================================================================
     *  /COPY required constants
     *=========================================================================

     * Form ID
     D xxform          c                   const('PP1A      ')
    D***  Begin Add  ***  03/09/06  **************************************
     D prtPgm          c                   const('PP1A      ')
    D***  End   Add  ***  03/09/06  **************************************

     * Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    D***  Begin Add  ***  03/09/06  **************************************
     D #perPage        s              3  0 inz(10)
    D***  End   Add  ***  03/09/06  **************************************


    
     *=========================================================================
     *  PLIST definitions
     *=========================================================================
    C*
    C***  Begin Add  ***  03/09/06  **************************************
     C     prtPL         plist
     C                   parm                    qqMode
     C                   parm                    qqCo#
     C                   parm                    qqFnd
     C                   parm                    qqEmp#
     C                   parm                    qqDiv
     C                   parm                    qqFyr
    C***  End   Add  ***  03/09/06  **************************************

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
    C*
    C***  Begin Add  ***  03/09/06  **************************************
     C     *dtaara       define    w7413pdf      xxpdf             1
    C*
     C                   in        xxpdf
    C*
     C                   select
     C                   when      xxpdf = 'Y'
     C                   eval      #perPage = 6
     C                   other
     C                   endsl
    C***  End   Add  ***  03/09/06  **************************************
    C***  End   Add  ***  03/09/06  **************************************
    
    
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
      *
      *  Owner / Officer
      *
     C     ooky          KList
     C                   KFld                    emCO#
     C                   KFld                    emFND
     C                   KFld                    emEMP#
     C                   KFld                    emDIV
    
    
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     * Copybook C specs
     C/COPY SCOPYEN01C
    
    
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    
     c                   movel(p)  xxform        fmform
    
      * employer name
    c                   clear                   enamds
     c                   eval      enamds = emname + emnam2
    
      * Type of Business
     c                   clear                   fmc201
    
     c                   if        emtpob = '1'
     c                   eval      fmc201 = 'Sole Proprietor'
     c                   else
     c                   eval      fmc201 = 'Partnership'
     c                   endif
    
      * assume coverage is elected
     c                   eval      fmfl01 = 'X'
     c                   eval      fmfl02 = ' '
    
      * Owner / Officer Names
     c                   eval      x1 = 1
     c                   clear                   nam
     c                   clear                   InEx
     c     ooky          setll     poo01
     c     ooky          reade     poo01
    
     c                   dow       not %eof
    
     c                   if        (ooefdt <= elpfdt)
    
     c                   if        (ooexdt > elpfdt)
     c                             or (ooexdt = 0)

      *     all owner / officers must be included for elected coverage = 'X'
     c                   if        (ooexfl <> 'I')
     c                   eval      fmfl01 = *blank
     c                   eval      fmfl02 = 'X'
     c                   endif
    
     c                   eval      nam(x1) = %trim(ooofsn) + ' '
     c                                       + %trim(ooolsn)

     c                   select

     c                   when      ooexfl = 'I'
     c                   eval      InEx(x1) = 'Include'

     c                   when      ooexfl = 'E'
     c                   eval      InEx(x1) = 'Exclude'

     c                   other
     c                   eval      InEx(x1) = 'Exempt'

     c                   endsl

    C***  Begin Chg  ***  03/09/06  **************************************
     c**********         if        x1 >= 10
     c                   if        x1 >= #perPage
    C***  End   Chg  ***  03/09/06  **************************************
     c                   write     pfm01
     c                   eval      x1 = 1
     c                   clear                   nam
     c                   clear                   InEx

     c                   else

     c                   eval      x1 = x1 + 1
     c                   endif
    
     c                   endif
    
     c                   endif
    
     c     ooky          reade     poo01
     c                   enddo
    
     c                   if        nam(1) <> *blanks
     c                   write     pfm01
     c                   endif
    
    C***  Begin Add  ***  03/09/06  **************************************
     C                   select
     C                   when      xxpdf = 'Y'
     C                   call      prtPgm        prtPL
     C                   move      'P'           qqmode
     C                   other
     C                   endsl
    C***  End   Add  ***  03/09/06  **************************************
    
     C                   endsr
    
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     c     pgmstr        begsr
    
     c                   endsr
    
    
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     *
     C                   endsr
    
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     c     valid         begsr
    
     * Type of business = "Sole Proprieter" or "Partnership"
    
     c                   if        (emtpob = '1')
     c                             or (emtpob = '2')
     c                             or (emtpob = 'D')
     c                             or (emtpob = '6')
    
     * NJ coverage for state reporting division
    
     c     rsky          chain     prs01                              60
     *
     c                   if        (*in60 = *off)
     c                             and (rsjrst = 'NJ')
    
     * Effective Owner / Officer
    
     c     ooky          setll     poo01
     c     ooky          reade     poo01
    
     c                   dow       not %eof
    
     c                   if        (ooefdt <= elpfdt)
    
     c                   if        (ooexdt > elpfdt)
     c                             or (ooexdt = 0)
     C                   move      *on           crtfrm
     c                   endif
    
     c                   endif
    
     c     ooky          reade     poo01
     c                   enddo
     C                   endif
     *
     C                   endif
     *
     C                   endsr
    
    
    
