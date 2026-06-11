     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H bnddir('WTBNDDIR')
    H/TITLE SR04104 - Terrorism Risk Insurance Extension Act End - CA
     *===============================================================*
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 02/29/08                                          *
     * Programmer: C:197                                             *
     * Project   : WC1667                                            *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
      * 10/27/22 C:333 Project: WC2834 (Recompile)                    *
     *                Endorsement Recognition of Cancellation        *
     *                                                               *
      * 12/19/16 C:180 Project SY371 scriptura                        *
     *                                                               *
     * 07/18/08 C:190 SEQ#64396                                      *
      *                Changed the processing for the Terrorism Endor-*
     *                sement so that it attaches without checking    *
     *                Foreign or Domestic Terrorism End Stat Code    *
      *                in Stat Rpt/Rating.                            *
     *                                                               *
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
    
     * Copybook F specs
     F/COPY SCOPYEN01F
    
      ** Statistical Reporting/Rating File
     Fwdrrl     if   e           k disk
     *=========================================================================
     *  Definition Specs
     *=========================================================================

      **** Begin Add **** 12/19/16 *********************************************
      * Entry
     D SR04104         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR04104         PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1
      **** End   Add **** 12/19/16 *********************************************
     * Copybook D specs
     D/COPY SCOPYEN01D

      *


     *=========================================================================
     *  /COPY required constants
     *=========================================================================

     * Form ID
      **** Begin Change *** 12/19/16 ********************************
     D*** xxform          c                   const('WC040104  ')
     D qqform          c                   const('WC040104       ')
      **** End   Change *** 12/19/16 ********************************

     * Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')



     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
    
    
    
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

      ****** BEGIN DELETE **** 12/19/16 ****************************
     ** Entry parameters
     C***  *entry        plist
     C***                parm                    qqco#
     C***                parm                    qqfnd
     C***                parm                    qqemp#
     C***                parm                    qqdiv
     C***                parm                    qqfyr
     C***                parm                    qqmode
     C***                parm                    qqrrn
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
      ****** END   DELETE **** 12/19/16 ****************************
    
    
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
      *
      *  Statistical Reporting/Rating
      *
     C     rrky          KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    RSJRST
     C                   KFld                    ELPFDT
      *
     C     rrky1         KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    RSJRST
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
     *=========================================================================
     *  CKSTAT - Check if endorsement approved for state                   d
     *=========================================================================
     c     ckstat        begsr
    
     c                   move      'N'           approved_state    1
    
     c                   if        (rsjrst = 'CA')
    
     c                   eval      approved_state = 'Y'

     c                   endif

     c                   endsr
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    
     ** Write the output record
      *** BEGIN DELETE ****** 12/19/16 *************************************
     C***                write     pfm01
      *** END   DELETE ****** 12/19/16 *************************************
    
     C                   endsr
    
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     C*
      **** Begin Add *** 12/19/16 *****************************************
      /Free
       RunningSRtext = *ON;
      /End-Free
      **** End   Add *** 12/19/16 *****************************************
    
     C                   endsr
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
     c     rsky          chain     prs01                              60
     c                   exsr      ckstat
    
     c                   if        (*in60 = *off)
     c                             and (approved_state = 'Y')
     c
     c
     c     rrky          setgt     prr01
     c     rrky1         readpe    prr01                                  60
     *
     *  Attach endorsement if non-zero terror rate for jurisdiction state
     *
     c                   if        (*in60 = *off)
    C*** BEG CHG ******* 07/18/08 ******************************************
     c                             and (rrterc <> *Zeros)
     c***************              and (rrterr <> *Zeros)
    C*** END CHG ******* 07/18/08 ******************************************
     c                   move      *on           crtfrm
     c                   endif
     c                   endif
    
     c                   endsr
