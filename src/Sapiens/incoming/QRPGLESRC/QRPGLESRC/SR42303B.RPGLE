     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

     H/TITLE SR42303B - TX - Volunteer Workers Endorsement
      *------------------------------------------------------------- *
      *WC420303B -Texas Volunteer Workers Coverage Endorsement     *
      *                                                               *
      *Program Description:                                          *
      *                                                               *
      *   "Print/Attach" program for single manual attach endorsement *
      *   with addtional input program that stores information        *
      *   in generic endorsement data file wledp.                     *
      *                                                               *
      *------------------------------------------------------------- *
      *Revisions:                                                    *
     *                                                               *
     * 10/19/22 C:342 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
     * 09/15/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
      *                                                               *
      * 03/25/13 C:287 Project WC2194                                 *
      *                Created.                                       *
      *                                                               *
      *------------------------------------------------------------- *
      *Notes:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      *                                                               *
      *  QQMODE = 1 - Called to determine if form should be attached. *
      *  QQMODE = 2 - Called to print endorsement.                    *
      *  QQMODE = 3 - Called for additional input.                    *
      *  QQMODE = 4 - Called for mid-term processing.                 *
      *------------------------------------------------------------- *
    
      **Copybook F specs
     F/COPY SCOPYEN01F
    
      **Additional Information File
     Fwledl     if   e           k disk    usropn
     F                                     prefix(ed:2)
    
      *------------------------------------------------------------- *
      * Definition Specs
      *------------------------------------------------------------- *

      **Copybook D specs
     D/COPY SCOPYEN01D
    
      *------------------------------------------------------------- *
      * /COPY required constants
      *------------------------------------------------------------- *
    
      **Form ID
     D qqForm          s             15a   inz('WC420303B ')
    
      **Additional entry program
     D addpgm          c                   const('SR42303BI ')

      *entry parameters
     D SR42303B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR42303B        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      *------------------------------------------------------------- *
      * KLIST definitions
      *------------------------------------------------------------- *

      **Additional Input
     C     edky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxfyr
     C                   kfld                    xxForm

      *------------------------------------------------------------- *
      * Main
      *------------------------------------------------------------- *

      **Copybook C specs
     C/COPY SCOPYEN01C

     C     CVTDATSR      Begsr
     C                   Call      'SRCVTDAT'
  122C                   parm                    srdti
  122C                   parm      '*CYMD'       srffmt
  123C                   parm      '*MDY'        srtfmt
  123C                   parm                    srdto
  123C                   parm                    srerr
     C                   Endsr
    
      /Free
        //----------------------------------------------------------------------- *
        // OTHINF- Subroutine to load form specific information & write record
        //----------------------------------------------------------------------- *
           Begsr othinf ;
    
             Open wledl ;
             Chain edky ped01 ;

               If %found( wledl ) ;
                  FMCO# = edco# ;
                  FMFND = edfnd ;
                  FMFYR = edfyr ;
                  FMEMP# = edemp# ;
                  FMDIV = eddiv ;
                  FMFORM = edform ;
                  FMEMNM = edemnm ;
                  FMCARR = edcarr ;
                  FM2001 = ed2001 ;
                  FM2002 = ed2002 ;
                  FM2003 = ed2003 ;
                  FM2004 = ed2004 ;
                  FM2005 = ed2005 ;
                  FM2006 = ed2006 ;
                  FM2007 = ed2007 ;
                  FM2008 = ed2008 ;
                  FM2009 = ed2009 ;
                  FM2010 = ed2010 ;
                  FM2011 = ed2011 ;
                  FM2012 = ed2012 ;
                  FM2013 = ed2013 ;
                  FM2014 = ed2014 ;
                  FM2015 = ed2015 ;
                  FM2016 = ed2016 ;
                  FM2017 = ed2017 ;
                  FM2018 = ed2018 ;
                  FM2019 = ed2019 ;
                  FM2020 = ed2020 ;
                  FM2021 = ed2021 ;
                  FM2022 = ed2022 ;
                  FM2023 = ed2023 ;
                  FM2024 = ed2024 ;
                  FM2025 = ed2025 ;
                  FM2026 = ed2026 ;
                  FM2027 = ed2027 ;
                  FM2028 = ed2028 ;
                  FM2029 = ed2029 ;
                  FM2030 = ed2030 ;

               Endif ;

             Close wledl ;
    
           Endsr ;
    
        //----------------------------------------------------------------------- *
        // PGMSTR- Beginning of program processing
        //----------------------------------------------------------------------- *
           Begsr pgmstr ;

           Endsr ;
    
        //----------------------------------------------------------------------- *
        // PGMEND- End of program processing
        //----------------------------------------------------------------------- *
           Begsr pgmend ;

           Endsr ;
    
        //----------------------------------------------------------------------- *
        // VALID- Subroutine to attach this form to the policy
        //----------------------------------------------------------------------- *
           Begsr valid ;
              crtfrm = *On ;
           Endsr ;
    
