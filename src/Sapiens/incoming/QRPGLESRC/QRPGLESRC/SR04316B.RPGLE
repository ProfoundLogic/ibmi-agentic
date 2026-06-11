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
      * 10/10/17 C:136 Project: 150WBL                                *
      *                Use longer field in workfile for name          *
      *                                                               *
      * 07/25/17 C:180 Project: 150WAP                                *
      *                Handle groups with combined PEOs.              *
      *                                                               *
      * 05/02/17 C:180   - Populate WXPEP record with client data.   *
      *                  - Scrub the name and address fields for     *
      *                    punctuation                               *
     *                                                               *
     * 03/28/17 C:180 PROJECT WC2481                                 *
     *                Convert to Auto-attach.                        *
     *                                                               *
     * 06/09/08 C:61  PROJECT 136WDS                                 *
     *                Created.                                       *
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
      *** Begin Delete *** 03/28/17 ***************************
     F***WLl2L     if   e           k disk
      *** End   Delete *** 03/28/17 ***************************
     f*** Begin Add ***  05/02/17  *************************************
     FWXPEL     UF   E           K DISK
     FWXPEL1    IF   E           K DISK    RENAME(PPE01:PPE02)
     f*** End Add *****  05/02/17  *************************************

     ** Copybook F specs
     F/COPY SCOPYEN01F


     *=========================================================================
     *  Definition Specs
     *=========================================================================


     C*** begin del ***  03/02/17  ***************************************
     D***** ##call          s              1
     D***** ##kc            s              1
     D***** ##kl            s              1
     D***** ##rrn           s              9  0
     D***** ##rrn2          s              9  0
     D***** ##view          s              1  0
     C*** end del *****  03/02/17  ***************************************

     C*** begin add ***  03/02/17  ***************************************
     D SR04316B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR04316B        PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1
     C*** end add *****  03/02/17  ***************************************

     ** Copybook D specs

     D/COPY SCOPYEN01D
      *** Begin Add    03/28/17 ************************************
     D/COPY WPRPEO
      //***** Beg Del *** 10/28/22 **********************************
     D*/Copy WprEmplr
      //***** End Del *** 10/28/22 **********************************
      *** End   Add    03/28/17 ************************************
     d*** begin add ***  04/21/17  *************************************
     D/COPY SPRSTRING
     D/Copy wWCPOLDAT4
     D xxunid          s                   like(PEUNID)
     D x1end#          s                   like(PEEND#)
     d*** end add *****  04/21/17  *************************************        

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
     C*** begin chg ***  03/02/17  ***************************************
     D***** xxform          c                   const('WC040316  ')
     D qqform          s             15a   inz('WC040316B ')
     C*** end chg *****  03/02/17  ***************************************

     ** Additional entry program
      *** Begin Change 03/28/17 ************************************
     D**** addpgm          c                   const('SR04316I  ')
     D addpgm          c                   const('SRXXXXXX  ')
      *** End   Change 03/28/17 ************************************


     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters
     C*** begin del ***  03/02/17  ***************************************
     C**** *entry        plist
     C****               parm                    qqco#
     C****               parm                    qqfnd
     C****               parm                    qqemp#
     C****               parm                    qqdiv
     C****               parm                    qqfyr
     C****               parm                    qqmode
      ****                                  wdelp
     C****               parm                    qqrrn
     C****               parm                    qqmod2
    
     ** Additional input program parameters
    
     C**** addpl         pList
     C****               parm      *off          ##kc
     C****               parm      *off          ##kl
     C****               parm      qqmod2        ##call
      ****                                  additional input data file
     C****               parm      0             ##rrn
      ****                                  wdelp
     C****               parm      qqrrn         ##rrn2
     C*** end del *****  03/02/17  ***************************************


     *=========================================================================
     *  KLIST definitions
     *=========================================================================

     C     l2ky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#

     C*** Begin Add ***  05/02/17  *************************************
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
     C*** End Add *****  05/02/17  *************************************

     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs (PLISTs, KLISTs and mainline)
     C/COPY SCOPYEN01C

      *** Begin Change *** 03/28/17 ****************************************
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information
     *=========================================================================
     C**   othinf        begsr
    
     C*** begin chg ***  03/02/17  ************************************
     C*****              z-add     1             i                 1 0
     C*****              z-add     1             i
     C*** end chg *****  03/02/17  ************************************
     C***  l2ky          setll     pl201
     C***  l2ky          reade     pl201                                  60
    
     C***                dow       not %eof
      *
      * move information to WXFMP fields to print on endorsement
      *
     C***                movel(p)  l2nam1        nam1
     C***                movel(p)  l2nam2        nam2
     C***                movel(p)  l2adr1        fmnm01
     C***                movel(p)  l2adr2        fmnm02
     C***                movel(p)  l2adr3        fmnm03
     C***                movel(p)  l2adr4        fmnm04
    
     C***                Exsr      WriteWorkFile
      *
      *  clear data fields
      *
     C***                clear                   fmc601
     C***                clear                   fmc602
     C***                clear                   fmnm01
     C***                clear                   fmnm02
     C***                clear                   fmnm03
     C***                clear                   fmnm04
      *
     C***  l2ky          reade     pl201                                  60
     C***                enddo
    
     C***                endsr
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
       //*** Begin add ***  05/02/17  ****************************************
       //*
       //* Get latest UNID
       //*
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
           //*** End   add ***  05/02/17  ****************************************
       Endsr;
      /End-Free
      *** End   Change *** 03/28/17 ****************************************
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
