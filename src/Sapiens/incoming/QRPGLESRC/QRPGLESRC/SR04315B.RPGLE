     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')

    H/TITLE SR04315B - CA - Labor Contractor as Named Insured with LCF
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
     * WC040315B - Endorsement Limiting and Restricting this Insurance
     *            Labor Contractor as Named Insured with LCF designation
     *            Restriction of Coverage to Client Workers
     *
      *   Single Manual attached endorsement
     *   with no additional information entry needed                 *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     *===============================================================*
     * REVISIONS:                                                    *
      *                                                               *
      * 11/04/24 C:342 Support: Phw#117924 (Prescient)                *
      *                Changed Address fields for WC040315B form.     *
      *                was using : FMEAD1, FMEAD2, and FMEAD3         *
      *                using now : FMC401, FMC402, and FMC403         *
      *                This need some Java changes as well by Abhi    *
     *                                                               *
      * 10/17/23 C:65  Project: Phw#111872 (Amwins)                   *
      *                Change reporting of client name to use the     *
      *                federal name if it exists.                     *
     *                                                               *
      * 10/28/22 C:333 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
     *                                                               *
      * 08/20/18 C:267 Project WC2634                                 *
      *                Add processing to write endorsement data to    *
     *                the WXPEP reporting file for CA state.         *
     *                                                               *
      * 03/29/18 C:65  Project 150WAD/Phw#70035                       *
      *                Read WDRRP for the output control flag.        *
     *                                                               *
     *                                                               *
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
     fwdrrl     if   e           k disk

      *** Begin Add  ***** 08/20/18 *********************************************
      * WCPOLS Endorsement transaction
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
      *** End   Add  ***** 08/20/18 *********************************************
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
      *** Begin Add  ***** 08/20/18 *********************************************
    D/COPY WWCPOLDAT4
      *** End   Add  ***** 08/20/18 *********************************************
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqForm          s             15a   inz('WC040315B ')
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    

      *** Begin Add  ***** 08/20/18 *********************************************
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)
     D strName         s            100
      *** End   Add  ***** 08/20/18 *********************************************
    
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
    
     *entry parameters
     D SR04315B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR04315B        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
    
    
     c     emky2         klist
     c                   KFld                    elco#
     c                   KFld                    elfnd
     c                   KFld                    elemp#
     c                   KFld                    eldiv
    
      *** Begin Add  ***** 08/20/18 *********************************************
     c     emky1         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxemp#
     c                   kfld                    xxdiv

      ** WXPEL - Policy page endorsement transaction file
     C     pekey         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxdt              7 0

     C     pekey1        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr

     C     pekey2        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxunid
     C                   KFld                    x1end#
    
      *** End   Add  ***** 08/20/18 *********************************************

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
     c     othinf        begsr
    
     * Employer Name (split to print on two 60 char lines, if needed)
    
     c                   movel(p)  fminnm        ##line
     c                   eval      ##length = 60

     c                   call      'SRLINE'
     c                   parm                    ##line          256
     c                   parm                    ##length          3 0
     c                   parm                    ##remain        256
    
     c                   movel(p)  ##line        fmc601
     c                   movel(p)  ##remain      fmc602
     c**** Print Federal name.
     c                   movel(p)  fmifnm        ##line
     c                   eval      ##length = 60

     c                   call      'SRLINE'
     c                   parm                    ##line          256
     c                   parm                    ##length          3 0
     c                   parm                    ##remain        256
    
     c                   movel(p)  ##line        fm2001
     c                   movel(p)  ##remain      fm2002
    
      *** Beg Add  ******* 08/20/18 *********************************************
     c**** Get Client name and address
     c                   clear                   E04315
      /free
    
       // Get client address info
       chain emky1 pem01;
       if %found(WMEML);
          If EMpad1 <> *blanks;
             xkadr1    = %trim(EMpad1);
             xkadrcty  = %trim(EMpcty);
             xkadrstat = EMpst;
             xkadrzip  = %editc(EMpzip:'X');
             //*** Beg Add ***  11/04/24  ***********************************
             FMc401    = %trim(EMpad1);
             FMc402    = %trim(EMpad2);
             FMc403    = %Trim(EMpcty) + ', ' + %Trim(EMpst) + ' ' +
                         %Subst(%EditC(EMpzip:'X'):1:5);
             If %Subst(%EditC(EMpzip:'X'):6:4) <> '0000';
                FMc403 = %Trim(FMc403) + '-' +
                 %Subst(%Editc(EMpzip:'X'):6:4);
             Endif;
             //*** End Add ***  11/04/24  ***********************************
          Else;
             xkadr1    = EMmad1;
             xkadrcty  = %trim(EMmcty);
             xkadrstat = EMmst;
             xkadrzip  = %editc(EMmzip:'X');
             //*** Beg Add ***  11/04/24  ***********************************
             FMc401    = %trim(EMmad1);
             FMc402    = %trim(EMmad2);
             FMc403    = %Trim(EMmcty) + ', ' + %Trim(EMmst) + ' ' +
                         %Subst(%EditC(EMmzip:'X'):1:5);
             If %Subst(%EditC(EMmzip:'X'):6:4) <> '0000';
                FMc403 = %Trim(FMc403) + '-' +
                 %Subst(%Editc(EMmzip:'X'):6:4);
             Endif;
             //*** End Add ***  11/04/24  ***********************************
          EndIf;
          If EMname <> *blanks;
             If Emnam2 <> *blanks;
                strName = %trim(Emname) + %trim(Emnam2);
             Else;
                strName = %trim(Emname);
             EndIf;
          EndIf;
          //*** begin add ***  10/17/23  ***********************************
          If EMFdnm <> *blanks;
             If EMFdn2 <> *blanks;
                strName = %trim(EMFdnm) + %trim(EMFdn2);
             Else;
                strName = %trim(EMFdnm);
             EndIf;
          EndIf;
          //*** end add *****  10/17/23  ***********************************
      /end-free
     c                   movel(p)  strName       ##line
     c                   eval      ##length = 79

     c                   call      'SRLINE'
     c                   parm                    ##line          256
     c                   parm                    ##length          3 0
     c                   parm                    ##remain        256
     /free
          xkNmCl = %trim(##line);
          xkseq# = '01';

          ExSr wrtWXPep;

          If ##remain <> *blanks;
             clear E04315;
             xkNmCl = %trim(##remain);
             xkseq# = '02';
             ExSr wrtWXPep;
          EndIf;

       EndIf;
      /end-free
      *** End Add  ******* 08/20/18 *********************************************
     c                   endsr
    
    
      *** Beg Add  ******* 08/20/18 *********************************************
     *=========================================================================
     *  wrtWXPep - Update Endorsement Reporting File for WCPOLS
     *=========================================================================
     C     wrtWXPep      begsr

      *  If printing a policy page.  (Not called from W1054R)

     c                   call      'SRCHKPGM'
     c                   parm      'W1054R   '   #1pgm            10
     c                   parm      *BLANK        #1flag            1

     C                   if        #1flag = '0'

      * Update the endorsement reporting file with the proper data.

     C                   Z-ADD     0             xxunid
     c                   move      *hival        xxdt
     C     pekey         setgt     ppe02
     C     pekey1        readpe    ppe02                                  60

     C                   If        *in60 = *off
     C                   z-add     peunid        xxunid
     C                   endif

     C                   movel     xxform        x1end#
     c     pekey2        chain     ppe01                              60
      *
      * Copy data into the record.
      *
     C                   If        *in60 = *off

     c                   move      *blanks       pedata
     c                   movel     E04315        pedata
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt
      *
     c                   if        xkseq# = '01'
     C                   update    ppe01
     c                   else

     c                   movel     q1user        peiusr
     c                   movel     q1pgm         peipgm
     c                   z-add     dstime        peitim
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         peidt
     C                   write     ppe01

     c                   endif
     c                   endif                                                  *in60 ifeq off
     C                   endif

     C                   endsr
      *** End Add  ******* 08/20/18 *********************************************
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     C                   endsr
    
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
    
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
    
     C*
      /free
       //*** begin add ***  03/29/18  *************************************
       Chain (xxCo#:ELrtcd) WMRSL;
       If %Found;

         Setgt (ELco#:ELfnd:RSjrst:ELPfdt) WDRRL;
         Readpe (ELco#:ELfnd:RSjrst) WDRRL;
         If not %eof;
       //*** end add *****  03/29/18  *************************************
           If EMlsid = '5' and RRpgfl = '3';
              Crtfrm = *on;
           Endif;
       //*** begin add ***  03/29/18  *************************************
         Endif;
       Endif;
       //*** end add *****  03/29/18  *************************************
      /End-Free
    
     C                   endsr
    
