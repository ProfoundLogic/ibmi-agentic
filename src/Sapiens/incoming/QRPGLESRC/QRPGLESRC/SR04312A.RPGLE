     hCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')
    
    H/TITLE SR04312A - CA - Employee Leasing Endorsement
     *===============================================================*
     * WC040312A                                                     *
     *  (For Leasing Arrangements that do not fall within the scope  *
     *   of the Employee Leasing Rule)                               *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 06/09/04                                          *
     * Programmer: C:61                                              *
     * Project   : CM027                                             *
     *===============================================================*
     * REVISIONS:                                                    *
      *                                                               *
      * 10/28/22 C:333 Project: WC2834                                *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      * 10/06/17 C:062 Project: 150WAP                                *
      *                Remove peo stuff from valid                    *
     *                                                               *
      * 07/25/17 C:180 Project: 150WAP                                *
      *                Handle groups with combined PEOs               *
      *                                                               *
      * 03/03/17 c:180 Project WC2481
      *                Add PEO info.

      * 03/02/17 c:62  Project WC2481
      *                Add scriptura processing
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: Please add comments to the top of revisions.           *
     * ------------------------------------------------------------- *
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
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     D/COPY SCOPYEN01D
     D/COPY WPRPEO
      //***** Beg Del *** 10/28/22 **********************************
     D*/Copy WprEmplr
      //***** End Del *** 10/28/22 **********************************
    
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D*** Begin Chg *** 03/02/17 ***********************************
     D*xxform          c                   const('WC040312A ')
     D qqform          c                   const('WC040312A ')
     D*** End   Chg *** 03/02/17 ***********************************
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    
     D*** Begin Add *** 03/02/17 ******************************
     *entry parameters
     D SR04312A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR04312A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
     D*** End   Add *** 03/02/17 ******************************
    
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
    
    
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     C*** Begin Del *** 03/02/17 **************************************
     ** Entry parameters
     C******entry        plist
     C**********         parm                    qqco#
     C**********         parm                    qqfnd
     C**********         parm                    qqemp#
     C**********         parm                    qqdiv
     C**********         parm                    qqfyr
     C**********         parm                    qqmode
     C**********         parm                    qqrrn
     C**********         parm                    qqmod2
     C*** End   Del *** 03/02/17 **************************************
    
     ** Additional input program parameters
     C*** Begin Del *** 03/02/17 **************************
     C*****addpl         pList
     C**********         parm      *blanks       qqprds
     C**********         parm      *blanks       qqcall
     C**********         parm                    qqco#
     C**********         parm                    qqfnd
     C**********         parm                    qqemp#
     C**********         parm                    qqdiv
     C**********         parm                    qqfyr
     C*** End   Del *** 03/02/17 **************************
    
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
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
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
      /Free
       Begsr Othinf;

        //Contractor's name and address

          dsPeoInfo = GetPeoInfo(xxco#:xxfnd);
          FM2003 = dsPeoInfo.Name;
          FM2004 = dsPeoInfo.FedName;
          FMC401 = dsPeoInfo.Address1;
          FMC402 = dsPeoInfo.Address2;
          FMC403 = dsPeoInfo.Address3;


       //Get Client's information

       setll (xxco#:xxfnd:xxfyr:xxemp#) pel02;
       reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;

       Dow not %eof;
       //***** Beg Add *** 10/28/22 ******************************
          xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
       //***** End Add *** 10/28/22 ******************************
             FM2001 = *blanks;
             FM2002 = *blanks;
             FMead1 = *blanks;
             FMead2 = *blanks;
             FMead3 = *blanks;

             Select;
             When FNpeof = 'M' and ELemp# = 1;
             When FNpeof = 'S' and ELdiv = 0;
             //*** Begin add *** 07/25/17 ************************************
             When FNpeof = 'C' and ELemp# = 1 and ELdiv = 0;
             //*** End   add *** 07/25/17 ************************************
             Other;
                Chain (Elco#:ELrtcd) prs01;
                If RSjrst = ENst;
                   If ELpfdt <> GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
                      dsCltInfo = GetCltInfo(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
                      FM2001 = dsCltInfo.fedName;
                      FM2002 = dsCltInfo.name;
                      FMead1 = dsCltInfo.address1;
                      FMead2 = dsCltInfo.address2;
                      FMead3 = dsCltInfo.address3;
                      Exsr WriteWorkFile;
                   Endif;
                Endif;
             Endsl;

          //***** Beg Add *** 10/28/22 ******************************
             Endif;
          //***** End Add *** 10/28/22 ******************************
          reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
          Enddo;
       Endsr;
      /End-Free
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
      /Free
       Begsr Pgmstr;
          WriteInCopybook = *Off;
       Endsr;
      /End-Free
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
    
     C*** Begin Del *** 03/02/17 *******************************************
     C*****rsky          chain     prs01                              60
    
     C**********         if        *in60 = *off
     C**********                   and rsjrst = enst
    
     ** Found a division with a matching jurisdiction state
     C**********         move      *on           crtfrm
     C**********         endif
     C*** End   Del *** 03/02/17 *******************************************
     C*** Begin Add *** 03/02/17 *******************************************
     C*** Begin Chg *** 10/06/17 ***************************************   ******
     C**********          If        crtFrm = *Off
     C**********          exsr      GetFnd
     C**********          If        FNpeoF <> ' '
     C**********          Eval      crtFrm = *On
     C**********          EndIf
     C**********          EndIf
     C                   Eval      crtFrm = *On
     C*** End   Chg *** 10/06/17 ***************************************   ******
     C*** End   Add *** 03/02/17 *******************************************
    
     C                   endsr
