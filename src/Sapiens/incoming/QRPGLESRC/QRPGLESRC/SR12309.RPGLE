     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/TITLE SR12309 - IL PEO Master Policy Endorsement - Ed. 01-17
     H*                Print/Attach program
      *===============================================================*
      * WC120309 - IL PEO Master Policy Endorsement                   *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      *===============================================================*
      * Created on 11/09/2017 by c:62 for Project WC2564              *
      *===============================================================*
      * Revisions:                                                    *
      *                                                               *
      * 09/20/24 C: 079 Project: WC2952                               *
      *                 Recompile                                     *
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
     D*** Beg Del *** 09/20/24 *********************************************
     D***/Copy WprEmplr
     D*** End Del *** 09/20/24 *********************************************
    
      *=========================================================================
      *  /COPY required constants
      *=========================================================================
    
      ** Form ID
     D qqform          s             15a   inz('WC120309  ')
    
      ** Additional entry program
     D addpgm          c                   const('SRXXXXXX  ')
    
      //Entry
     D SR12309         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR12309         PI
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

       //Get Client's information
       setll (xxco#:xxfnd:xxfyr:xxemp#) pel02;
       reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;

       Dow not %eof;
          Chain (Elco#:ELrtcd) prs01;

          Select;
          When FNpeof = 'M' And ELemp# = 1;
          When FNpeof = 'S' And ELdiv = 0;
          When FNpeof = 'C' And ELemp# = 1 And ELdiv = 0;

          Other;
             If RSjrst = ENst;
                If ELpfdt <> GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
                   dsCLTinfo = GetCltInfo(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
                   FM2001 = dsCltInfo.name;
                   FMc402 = dsCltInfo.address1;
                   If dsCltInfo.address3 = ' ';
                      FMc403 = dsCltInfo.address3;
                      FMc404 = dsCltInfo.address2;
                   Else;
                      FMc403 = dsCltInfo.address2;
                      FMc404 = dsCltInfo.address3;
                   EndIf;
                   FMfein = %dec(dsCltInfo.fein:9:0);
                   Exsr WriteWorkFile;
                Endif;
             Endif;
          Endsl;

          reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
          Enddo;

       EndSr;
       //=======================================================================
       //PGMSTR - Beginning of program processing
       //=======================================================================
       Begsr PgmStr;

          UseDfltAddParms = *Off;
          WriteInCopybook = *Off;

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
