     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR04316 - Agreement Limiting and Restricting
     *===============================================================*
     * PROGRAM DESCRIPTION:                                          *
     *                                                               *
     *  WC040316                                                     *
     *    Labor Contractor Client as Named Insured                   *
     *    Exclusion of Client's Employees                            *
     *                                                               *
     *  Print/Attach for Multiple Manual with Additional Input       *
     *  Endorsement (data stored in file specifed for endorsement)   *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
      * 10/28/22 C:333 Project: WC2834                                *
      *                Endorsement Recognition of Cancellation        *
     *                                                               *
      * 12/06/17 C:65  Project: 148WAF                                *
      *                Re-created from 'B' version.                   *
     *                                                               *
     *===============================================================*
     * NOTES:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
     *                                                               *
     *  qqmode = 1 - Called to determine if form should be attached. *
     *  qqmode = 2 - Called to print endorsement.                    *
     *  qqmode = 3 - Called for additional input.                    *
     *  qqmode = 4 - Called for midterm processing.                  *
     *===============================================================*

      * Agreement Limiting and Restricting Endorsement Form Data
     FWXPEL     UF   E           K DISK
     FWXPEL1    IF   E           K DISK    RENAME(PPE01:PPE02)

     ** Copybook F specs
     F/COPY SCOPYEN01F


     *=========================================================================
     *  Definition Specs
     *=========================================================================



     D SR04316         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR04316         PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1

     ** Copybook D specs

     D/COPY SCOPYEN01D
     D/COPY WPRPEO
      //***** Beg Del *** 10/28/22 ***********************************
     D*/Copy WprEmplr
      //***** End Del *** 10/28/22 ***********************************
     D/COPY SPRSTRING
     D/Copy wWCPOLDAT4
     D xxunid          s                   like(PEUNID)
     D x1end#          s                   like(PEEND#)

     d nam1            ds            60
     d  fmc401                             overlay(nam1:1)
     d  fmc201                             overlay(nam1:41)

     d nam2            ds            60
     d  fmc402                             overlay(nam2:1)
     d  fmc202                             overlay(nam2:41)

     *=========================================================================
     *  Copybook required constants
     *=========================================================================

     ** Form ID
     D qqform          s             15a   inz('WC040316  ')

     ** Additional entry program
     D addpgm          c                   const('SRXXXXXX  ')


     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters


     *=========================================================================
     *  KLIST definitions
     *=========================================================================

     C     l2ky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#

     C     PEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXDT              7 0

     C     PEKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR

     C     PEKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXUNID
     C                   KFLD                    X1END#

     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs (PLISTs, KLISTs and mainline)
     C/COPY SCOPYEN01C
    
      /Free
       //========================================================================
       //  OTHINF - Subroutine to load form specific information
       //========================================================================
       Begsr OthInf;
       dsPeoInfo = GetPeoInfo(ELco#:ELfnd);
       FMc401    = %subst(dsPeoInfo.name:1:40);
       FMc201    = %subst(dsPeoInfo.name:41:20);
       FM2001    = dsPeoInfo.name;
       FMc402    = %subst(dsPeoInfo.FedName:1:40);
       FMc202    = %subst(dsPeoInfo.FedName:41:20);
       FM2002    = dsPeoInfo.fedName;
       FMnm01    = dsPeoInfo.address1;
       FMnm02    = dsPeoInfo.address2;
       FMnm03    = dsPeoInfo.address3;
       If  dsPeoInfo.fein <> *blanks;
           FMfein  = %dec(dsPeoInfo.fein:9:0);
       Else;
          FMfein = *Zeros;
       Endif;
          xxdt = *hival;
          Setgt pekey ppe02;
          Readpe(n) pekey1 ppe02;
          If not %eof;
             xxunid = PEunid;
             x1end# = %trim(qqform);
             Chain pekey2 ppe01;                //Copy data into record
             If %found;
                Clear e04316;
                x9cname = %trim(ExtrAlphaNum(FMc401)) +
                          %trim(ExtrAlphaNum(FMc201)) + ' ' +
                          %trim(ExtrAlphaNum(FMc402)) +
                          %trim(ExtrAlphaNum(FMc202));

                If %subst(fmnm02:1:5) <> %subst(dsPeoInfo.city:1:5);
                   x9cadr = %trim(ExtrAlphaNum(fmnm01)) + ' ' +
                   %trim(ExtrAlphaNum(fmnm02));
                Else;
                   x9cadr = %trim(ExtrAlphaNum(fmnm01));
                Endif;

                x9ccity = %trim(ExtrAlphaNum(dsPeoInfo.city));
                x9cst   = dsPeoInfo.state;
                x9czip  = dsPeoInfo.zip;
                PEData = %trim(e04316);

                PEName = %trim(ExtrAlphaNum(PEName));
                Update ppe01;
             Endif;
          Endif;
       Endsr;
      /End-Free
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     C                   endsr
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr


     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
      /Free
       Begsr Valid;
       crtFrm = *On;
       Endsr;
      /End-Free
