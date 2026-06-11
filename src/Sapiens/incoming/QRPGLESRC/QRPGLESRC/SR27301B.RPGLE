     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
    H/TITLE SR27301B - NV Employee Leasing Multiple Coordinated
     H*                 Policies Basis Endorsement - Ed.3/16
    H*                 Print/Attach program
     *===============================================================*
     * WC270301B                                                     *
     *                                                               *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *===============================================================*
     * REVISIONS:                                                    *
      *
      *  09/20/24 C:79  Project: WC2952
      *                 Recompile
     *                                                               *
     * 10/06/17 C:136 Project: 150WBL                                *
     *               -PEO attachment criteria moved to copy code     *
     *               -Populate PEO name only                         *
     *                                                               *
      * 07/25/17 C:180 Project: 150WAP                                *
      *                Handle groups with combined PEOs.              *
      *                                                               *
     * 03/20/17  C:180 Project WC2481                                *
      *                 Make auto attach.                             *
      *                                                               *
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
     D qqform          s             15a   inz('WC270301B ')
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXX  ')
    
      //Entry
     D SR27301B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR27301B        PI
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

       dsPeoInfo = GetPeoInfo(xxco#:xxfnd);
       FM2001 = dsPeoInfo.Name;
       FM2002 = dsPeoInfo.FedName;
       FMC401 = dsPeoInfo.Address1;
       FMC402 = dsPeoInfo.Address2;
       FMC403 = dsPeoInfo.Address3;

       EndSr;
      //=======================================================================
      //PGMSTR - Beginning of program processing
      //=======================================================================
       Begsr PgmStr;
       UseDfltAddParms = *Off;
       Endsr;
      //=======================================================================
      //PGMEND - End of program processing
      //=======================================================================
       Begsr Pgmend;
       Endsr;
      //=======================================================================
      //VALID - Subroutine to attach this form to the policy
      //=======================================================================
       Begsr valid;
       crtFrm = *On;
       Endsr;
     /end-free
