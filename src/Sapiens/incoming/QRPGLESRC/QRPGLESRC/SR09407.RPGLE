     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
    H/TITLE SR09407 - Florida Endorsement WC090407 - Text
     * ====================================================================== *
     *                                                                        *
     *Program Description:                                                   *
     *                                                                        *
     *  Determines if this endorsement form should be attached to            *
     *  the policy and creates data in the Endorsement Form                  *
     *  Workfile (WXFMP) prior to a form merge.                              *
     *                                                                       *
     * Created   : 06/17/13                                                   *
     * Programmer: C:287                                                      *
     *Project   : WC2209                                                     *
     * ====================================================================== *
     *REVISIONS:                                                             *
     *                                                                        *
     * 10/31/22 C:333 Project: WC2834                                         *
      *                Endorsement Recognition of Cancellation
     *                                                                        *
     * 12/29/16 C:180 Project: SY371 Scriptura                                *
      *                Add RunningSRtext indicator.
     *                                                                        *
     * 10/14/16 C:136 Project: WC2070                                         *
     *                Implement Scriptura Phase 2 changes under               *
     *                WC2070 rather than SY371 because other state            *
      *                endorsements were included in this project and          *
      *                SY371 changes were implemented into them                *
     * ====================================================================== *
     *Notes:                                                                 *
     *                                                                        *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).                   *
     *                                                                        *
     *  QQMODE = 1 - Called to determine if form should be attached.          *
     *  QQMODE = 2 - Called to print endorsement.                             *
     *  QQMODE = 3 - Called for additional input.                             *
     *  QQMODE = 4 - Called for mid-term processing.                          *
     * ====================================================================== *
    
     **Copybook F specs
     f/copy scopyen01f
    
     **Copybook D specs
      //***** Beg Del *** 10/31/22 **********************************
     d***/copy wprEmplr
      //***** End Del *** 10/31/22 **********************************
    
      *Entry
     d SR09407         pr
     d                                3
     d                                3
     d                                9
     d                                5
     d                                3
     d                                1
     d                                9  0
     d                                1

     d SR09407         pi
     d  qqCo#                         3
     d  qqFnd                         3
     d  qqEmp#                        9
     d  qqDiv                         5
     d  qqFyr                         3
     d  qqMode                        1
     d  qqRrn                         9  0
     d  qqMod2                        1

     * ========================================================================
     * Definition Specs
     * ========================================================================

     **Copybook D specs
     d/copy scopyen01d
    
     * ========================================================================
     * /COPY required constants
     * ========================================================================
    
     **Form ID
     d qqform          s             15a   inz('WC090407')
    
     **Additional entry program
     d addPgm          c                   const('SRXXXXXXXX')
    
     * ========================================================================
     * Stand alone fields
     * ========================================================================
     d sched           s              1    inz(' ')
     d large           s              1    inz(' ')
     d retro           s              1    inz(' ')
    
     * ====================================================================
     *    Main
     * ====================================================================

     **Copybook C specs
     c/copy scopyen01c
    
     /Free

      // ========================================================================
      // OTHINF - Subroutine to load form specific information & write record
      // ========================================================================
       Begsr OthInf;
      Endsr;
      // ========================================================================
      // PGMSTR - Beginning of program processing
      // ========================================================================
       Begsr PgmStr ;
        RunningSRtext = *On;
       Endsr ;
      // ========================================================================
      // PGMEND - End of program processing
      // ========================================================================
       Begsr PgmEnd ;
       Endsr ;
      // ========================================================================
      // VALID - Subroutine to attach this form to the policy
      // ========================================================================
       Begsr valid ;

          GetEmpRating(ELco#:ELfnd:ELemp#:ELdiv:ELfyr:' ':sched:
           large:retro ) ;
          If retro <> 'Y' ;
             crtfrm = *On ;
          Endif ;

       Endsr ;

     /End-Free
