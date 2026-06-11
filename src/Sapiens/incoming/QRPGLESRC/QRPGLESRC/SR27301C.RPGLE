     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/TITLE SR27301C - NV Employee Leasing Multiple Coordinated
     H*                 Policies Basis Endorsement - Ed.6/12
     H*                 Print/Attach program
      *===============================================================*
      * WC270301C - NV Employee Leasing Multiple Coordinated          *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      *===============================================================*
      * Created on 10/25/2017 by c:62 for Project WC2571              *
      *===============================================================*
      * Revisions:                                                    *
      *                                                               *
      * 11/03/22 C:333 Project: WC2834                                *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      *===============================================================*
      * Notes:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      *                                                               *
      *  QQMODE = 1 - Called to determine if form should be attached. *
      *  QQMODE = 2 - Called to print endorsement.                    *
      *  QQMODE = 3 - Called for additional input.                    *
      *  QQMODE = 4 - Called for mid-term processing.                 *
      *===============================================================*
      ** Copybook F specs
     F/COPY SCOPYEN01F
    
      *=========================================================================
      *  Definition Specs
      *=========================================================================
      ** Copybook D specs
     D/COPY SCOPYEN01D
     D/COPY WPRPEO
      //***** Beg Del *** 11/03/22 **********************************
     D***/Copy WprEmplr
      //***** End Del *** 11/03/22 **********************************
    
      *=========================================================================
      *  /COPY required constants
      *=========================================================================
    
      ** Form ID
     D qqform          s             15a   inz('WC270301C ')
    
      ** Additional entry program
     D addpgm          c                   const('SRXXXXXX  ')
    
      //Entry
     D SR27301C        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR27301C        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

    
      *=========================================================================
      *  Main
      *=========================================================================
      ** Copybook C specs
     C/COPY SCOPYEN01C
    
      /free
       //=======================================================================
       //OTHINF - Subroutine to load form specific information & write record
       //=======================================================================
       Begsr othinf;

       dsPeoInfo = GetPeoInfo(xxco#:xxfnd);
       FM2001 = dsPeoInfo.Name;
       FM2002 = dsPeoInfo.FedName;
       FMC401 = dsPeoInfo.Address1;
       FMC402 = dsPeoInfo.Address2;
       FMC403 = dsPeoInfo.Address3;

       EndSr;
       //=======================================================================
       //PGMSTR - Beginning of program processing
       //=======================================================================
       Begsr PgmStr;
       UseDfltAddParms = *Off;
       Endsr;
       //=======================================================================
       //PGMEND - End of program processing
       //=======================================================================
       Begsr Pgmend;
       Endsr;
       //=======================================================================
       //VALID - Subroutine to attach this form to the policy
       //=======================================================================
       Begsr valid;
       crtFrm = *On;
       Endsr;
      /end-free
