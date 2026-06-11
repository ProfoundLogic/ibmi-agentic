     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')
      /TITLE SR04406D - CA - Farm Schedule Endorsement
      *===============================================================*
      * WC040406D                                                     *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 02/23/16                                          *
      * Programmer: C:302                                             *
      * Project   : WC2414                                            *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
      * 12/28/16 C:180 Project SY371 scriptura                        *
      *                Add Scriptura interface.                       *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of revisions.           *
      * ------------------------------------------------------------- *
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

      * Employer Payroll
     Fwmepl7    if   e           k disk

      ** Copybook D specs
     D/COPY SCOPYEN01D

      ** Form ID
      *** Begin Change *** 12/28/16 ********************************************
     D*** xxform          c                   const('WC040406D ')
     D QQFORM          S             15A   inz('WC040406D   ')
     D QQCo#           S              3    inz
     D QQFnd           S              3    inz
     D QQEmp#          S              9    inz
     D QQDiv           S              5    inz
     D QQFyr           S              3    inz
     D QQMode          S              1    inz
     D QQRrn           S              9  0 inz
     D QQMod2          S              1    inz
      *** End   Change *** 12/28/16 ********************************************

      ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')

     D SR04406D        PR
     D QQCo#                          3
     D QQFnd                          3
     D QQEmp#                         9
     D QQDiv                          5
     D QQFyr                          3
     D QQMode                         1
     D QQRrn                          9  0
     D QQMod2                         1

     D SR04406D        PI
     D  x1Co#                         3
     D  x1Fnd                         3
     D  x1Emp#                        9
     D  x1Div                         5
     D  x1Fyr                         3
     D  xxMode                        1
     D  xxRrn                         9  0
     D  xxMod2                        1

      ** Additional input program parameters
      ** Begin Delete *******   12/28/16 ***************************************
     C***  addpl         pList
     C***                parm      *blanks       qqprds
     C***                parm      *blanks       qqcall
     C***                parm                    qqco#
     C***                parm                    qqfnd
     C***                parm                    qqemp#
     C***                parm                    qqdiv
     C***                parm                    qqfyr
     *** End   Delete *******   12/28/16 ***************************************

      /Free

       qqCo# = x1Co#;
       qqFnd = x1Fnd;
       qqEmp# = x1Emp#;
       qqDiv = x1Div;
       qqFyr = x1Fyr;
       qqMode = xxMode;

       //* Copybook C specs
        /COPY SCOPYEN01C

       //=====================================================================
       //  OTHINF - Subroutine to load form specific information & write record
       //=====================================================================
       begsr othinf;
       //** Begin Delete *** 12/28/16 ***********************************   ****
       //*** Write pfm01;
       //** END   Delete *** 12/28/16 ***********************************   ****
       Endsr;

       //=====================================================================
       //  PGMSTR - Beginning of program processing
       //=====================================================================
       Begsr pgmstr;
        RunningSRtext = *On;
       Endsr;

       //=====================================================================
       //  PGMEND - End of program processing
       //=====================================================================
       Begsr pgmend;
       Endsr;

       //=====================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=====================================================================
       Begsr valid;

          Chain (xxCo#:ELrtcd) prs01;

          If %Found and RSjrst = ENst;
             Setll (elco#:elfnd:elfyr:elemp#:eldiv) wmepl7;
             Reade (elco#:elfnd:elfyr:elemp#:eldiv) wmepl7;

             Dow Not %EOF(wmepl7);

                If (epclas = 00792) or (epclas = 0079) or (epclas = 0044)
                  or (epclas = 0036) or (epclas = 00382) or (epclas = 0038)
                  or (epclas = 0171) or (epclas = 0016) or (epclas = 0045)
                  or (epclas = 0041) or (epclas = 0034) or (epclas = 00341)
                  or (epclas = 00342) or (epclas = 00381) or (epclas = 00791)
                  or (epclas = 0172) or (epclas = 0040)
                  or (epclas = 0005) or (epclas = 0035) or (epclas = 0050);
                   CrtFrm = *On;
                   Leave;
                Endif;

                reade (elco#:elfnd:elfyr:elemp#:eldiv) wmepl7;
             Enddo;

          Endif;

       Endsr;
      /End-Free
