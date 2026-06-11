     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
    H/TITLE SR27301A - NV Employee Leasing Endorsement
    H*                 Print/Attach program
     *===============================================================*
     * WC270301A                                                     *
     *                                                               *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *===============================================================*
     * REVISIONS:                                                    *
      //
      // 09/20/24 C:79  Project: WC2952
      //                Recompile
     *                                                               *
      * 07/25/17 C:180 Project: 150WAP                                *
      *                Handle groups with combined PEOs.              *
      *                                                               *
     * 03/20/17  C:180 Project WC2481                                *
      *                 Make auto attach.                             *
      *                                                               *
     * 02/03/17  C:267 Project WC2481                                *
     *           Cloned from SR27301                                 *
     *           use group 903 for multi testing                     *
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
     ** Copybook F specs
     F/COPY SCOPYEN01F
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================
     ** Copybook D specs
     D/COPY SCOPYEN01D
     D/COPY WPRPEO
     D*** Beg Del *** 09/20/24 *****************************************
     D***/Copy WprEmplr
     D*** End Del *** 09/20/24 *****************************************
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqform          s             15a   inz('WC270301A ')
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXX  ')
    
      //Entry
     D SR27301A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR27301A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

    
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
      /free
      //=======================================================================
      //OTHINF - Subroutine to load form specific information & write record
      //=======================================================================
       Begsr othinf;


       //Get Client Info

       setll (xxco#:xxfnd:xxfyr:xxemp#) pel02;
       reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;

       Dow not %eof;
          Chain (Elco#:ELrtcd) prs01;

          Select;
          When FNpeof = 'M' and ELemp# = 1;
          When FNpeof = 'S' and ELdiv = 0;
          //*** Begin Add *** 07/25/17 *******************************************
          When FNpeof = 'C' and ELemp# = 1 and ELdiv = *Zero;
         //*** End   Add *** 07/25/17 *******************************************
          Other;
             If RSjrst = ENst;
                If ELpfdt <> GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
                   FMC201 = dsCltInfo.Emplink;
                   FMC301 = dsCltInfo.name;
                   FMC302 = dsCltInfo.fedName;
                   FMC401 = dsCltInfo.address1;
                   FMC402 = dsCltInfo.address2;
                   FMC403 = dsCltInfo.address3;
                   FMC101 = dsCltInfo.Fein;
                   Exsr WriteWorkFile;
                Endif;
             Endif;
          Endsl;

          reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
          Enddo;

       EndSr;
      //=======================================================================
      //PGMSTR - Beginning of program processing
      //=======================================================================
       Begsr PgmStr;
         WriteInCopyBook = *Off;
         UseDfltAddParms = *Off;
       Endsr;

      //=======================================================================
      //PGMEND - End of program processing
      //=======================================================================
       Begsr Pgmend;
       Endsr;
    
      //=======================================================================
      //VALID - Subroutine to attach this form to the policy
      //=======================================================================
       Begsr valid;
          //*** Begin Add *** 07/25/17 *******************************************
         Exsr GetFnd;
          //*** End   Add *** 07/25/17 *******************************************

         if fnpeof <> *blanks;
          crtfrm = *on;
         endif;
       Endsr;
    
     /end-free
