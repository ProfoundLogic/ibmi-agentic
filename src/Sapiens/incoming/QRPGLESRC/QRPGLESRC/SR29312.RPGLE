     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR29312 - NJ Designated Governmental Entity Endorsement
     *===============================================================*
     * WC290312 - NJ Notice of Cancellation or Material Coverage     *
     *            Change to Designated Governmental Entity           *
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
     * 11/03/22 C:333 Project: WC2834 (Recompile)                    *
     *                Endorsement Recognition of Cancellation        *
     *                                                               *
     * 09/08/17 C:291 Project: WC2525                                *
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
      ** Endorsement Forms Document Print Work File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm0p1)
     Fwxfmp2    if   e           k disk    usropn
     F                                     rename(pfm02:pfm0p2)
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     * Copybook D specs
     D/COPY SCOPYEN01D

     d xxdat7          s                   like(elpfdt)

     *=========================================================================
     *  /COPY required constants
     *=========================================================================

     * Form ID
     D qqform          c                   const('WC290312  ')

     * Additional entry program
     D addpgm          c                   const('SR29312I  ')

     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
    
    
    
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqemp#            9
     C                   parm                    qqdiv             5
     C                   parm                    qqfyr             3
     C                   parm                    qqmode            1
     C                   parm                    qqrrn             9 0
     C                   parm                    qqmod2            1
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     * Copybook C specs
     C/COPY SCOPYEN01C
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    
      /Free
          open wxfmp1;
          chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:xxForm) wxfmp1;
          FMA701 = F3a701;                              //Number of Days
          close wxfmp1;
    
          open wxfmp2;
          chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:xxForm) wxfmp2;
          FM2001 = F4C701;                              //Name
          FM2002 = F4C702;                              //Address 1
          FM2003 = F4C703;                              //Address 2
          FM2004 = F4C704;                              //City
          FMCD01 = F4ST01;                              //State
          FMC101 = %editc(F4FEIN:'X');                  //Zip code
          close wxfmp2;
      /End-Free
    
     C                   endsr
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr

     c                   time                    dsdttm
    
     C                   endsr
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     *
     C                   endsr
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     c     valid         begsr
    
     c                   move      *on           crtfrm
    
     c                   endsr

