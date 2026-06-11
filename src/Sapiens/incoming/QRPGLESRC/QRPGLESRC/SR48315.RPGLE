     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR48315 - WI - Employee Leasing Company Endorsement (MCP)
     //==============================================================*
     // WC480315                                                     *
      //   Multiple automatically attached endorsement                *
     //                                                              *
     *  Program Description:                                         *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     *===============================================================*
     // Revisions:                                                   *
     //                                                              *
     // 10/19/22 C:342 Project: WC2834 (Recompile)                   *
     //                Endorsement Recognition of Cancellation       *
     //                                                              *
     // 10/06/17 C:136 Project: 150WBL                               *
     //                PEO attachment criteria moved to copy code    *
     //                                                              *
      // 08/01/17 C:180  Support: 48448                              *
      //                 - Fix FEIN                                  *
      //                                                             *
      // 04/21/17 C:180  Project: WC2488                             *
      //                 - Populate WXPEP record with client data.   *
      //                 - Scrub the name and address fields for     *
      //                   punctuation                               *
      //                 - Require employee leasing id value of '5'  *
      //                   instead of '4' as condition to attach.    *

     * 09/15/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -Use one field for PEO name in XML
      *               -No comments.
     *                                                               *
     * 04/07/11  C:190 Project WC1962                                *
     *                 Attach if 'Employee Leasing ID' has value     *
     *                 '4 = Client Company Only'.                    *
     *                                                               *
     * 12/20/10  C:220 Project WC1962                                *
     *                 Created.                                      *
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
    
     ** Copybook F specs
     F/COPY SCOPYEN01F
     f*** begin add ***  04/21/17  *************************************
     FWXPEL     UF   E           K DISK
     FWXPEL1    IF   E           K DISK    RENAME(PPE01:PPE02)
     f*** end add *****  04/21/17  *************************************
    fwledl     if   e           k disk
    fwdrrl     if   e           k disk
      * Employer Master
     Fwmeml3    if   e           k disk    rename(pem01:pem02)
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     D/COPY SCOPYEN01D

     d*** begin add ***  04/21/17  *************************************
     D/Copy wPrPeo
     D/COPY SPRSTRING
     D/Copy wWCPOLDAT4
     D xxunid          s                   like(PEUNID)
     D x1end#          s                   like(PEEND#)
     d*** end add *****  04/21/17  *************************************        
    
     d x               s              3  0 inz(0)

      * employer effective dates
     d edds            ds
     d  emefd1
     d  emefd2
     d  emefd3
     d  emefd4
     d  emefd5
    d ed                                  OVERLAY(edds)
     d                                     LIKE(emefd1) DIM(5)

      * employer cancel dates
     d cdds            ds
     d  emcad1
     d  emcad2
     d  emcad3
     d  emcad4
     d  emcad5
    d cd                                  OVERLAY(cdds)
     d                                     LIKE(emcad1) DIM(5)
      * name returned from SRADDR
     d emnam           ds
     d  ##emn1                       50
     d  ##emn2                       50
     d  ##emn3                       50
     d  ##emn4                       50
     d  ##emn5                       50
     d  ##emn6                       50
     d  ##emn7                       50
    d nam                                 OVERLAY(emnam)
     d                                     LIKE(##emn1) DIM(7)

     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqForm          s             15a   inz('WC480315  ')
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    D ##co#           s              3
    D ##fnd           s              3
    D ##emp#          s              9
    D ##div           s              5
    D ##odr           s              1
    D ##nodr          s              1
    D ##aodr          s              1
    D ##adrl          s              3  0
    D ##naml          s              3  0
    D ##fedl          s              3  0
    D ##fedl1         s              3  0
    D ##peo1          s             57
    D ##peo2          s             57
    D ##pgfl          s              1
    D #1fein          s                   like(emfein)
    
      *entry parameters
     D SR48315         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR48315         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    
     * Employer Master
    
     c     emky1         klist
     c                   KFld                    x1co#
     c                   KFld                    x1emp#
     c                   KFld                    x1div
     c                   KFld                    x1fnd
     c     emky2         klist
     c                   KFld                    elco#
     c                   KFld                    elfnd
     c                   KFld                    elemp#
     c                   KFld                    eldiv
    c*
     c     rrky1         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    rsjrst
     c                   kfld                    elpfdt
    c*
     c     rrky2         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    rsjrst
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs
     c/COPY SCOPYEN01C
     C*** begin add ***  04/21/17  *************************************
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
     C*** end add *****  04/21/17  *************************************
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     c     othinf        begsr
     c*
     c* for not master division
    c*
     c     elky2         Chain     pel02                              20
    c*
    c** Write the output record
    c*
     c                   clear                   fm2001
     c                   clear                   fm2002
     c                   clear                   fm2003
     c                   clear                   fmnm01
     c                   clear                   fmnm02
     c                   clear                   fmnm03
     c                   clear                   fmc101
    c*
    c* Get employer name
    c*
     c                   movel     elco#         ##CO#
     c                   movel     elfnd         ##FND
     c                   movel     elemp#        ##EMP#
     c                   movel     eldiv         ##DIV
     c                   movel     '1'           ##pgfl
    c*
    c* Get PEO employer name
    c*
     C                   call      'SRADDR'
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##emp#
     C                   parm                    ##div
     C                   parm      ' '           ##odr
     C                   parm      'F'           ##nodr
     C                   parm      'M'           ##aodr
     C                   parm                    ##emn1
     C                   parm                    ##emn2
     C                   parm                    ##emn3
     C                   parm                    ##emn4
     C                   parm                    ##emn5
     C                   parm                    ##emn6
     C                   parm                    ##emn7
     C                   parm      *zero         ##adrl
     C                   parm      *zero         ##naml
     C                   parm      *zero         ##fedl
     C                   parm      *blank        ##peo1
     C                   parm      *blank        ##peo2
     C                   parm                    ##pgfl
     C*
     C*** begin chg ***  04/27/17  *********************************************
     c*****              eval      fm2001 = ##peo1
     c*****              eval      fm2002 = ##peo2
     c*****              eval      fm2003 = ##peo1 + ##peo2
     c                   Eval      fm2001 = ##emn1
     c                   Eval      fm2003 = ##emn1
     C                   If        ##naml = 2
     c                   Eval      fm2002 = ##emn2
     c                   Eval      fm2003 = %trim(##emn1) + ' ' + %trim(##emn2)
     c                   Endif
     C*** end chg *****  04/27/17  *********************************************
    c*
    c* Get employer address if mailing / physical
    c*
     c                   eval      x =  ##adrl
     c                   if        x <> 0
    c*
     c                   movel(p)  nam(x)        fmnm01
     c                   eval      x =  x + 1
     c                   movel(p)  nam(x)        fmnm02
     c                   eval      x =  x + 1
     c                   movel(p)  nam(x)        fmnm03
    c*
     c                   endif
    c*
    c* Get employer FEIN
    c*
     C                   z-add     elco#         x1co#             3 0
     C                   z-add     elfnd         x1fnd             3 0
     C                   z-add     1             x1emp#            9 0
     C                   z-add     0             x1div             5 0
     C*
     c                   move      emfein        #1fein
     c     emky1         chain     pem02                              60
     C*
     c                   if        *in60 = *off
     C*
     c                   move      emfein        fmc101
     c                   else
     c                   move      #1fein        fmc101
     c                   endif
     c*
      /Free
       //*** Begin add ***  04/21/17  ****************************************
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
                Clear e48315;
                x8cname = %trim(ExtrAlphaNum(fm2003));

                dsPeoInfo = GetPeoInfo(ELco#:ELfnd);

                If %subst(fmnm02:1:5) <> %subst(dsPeoInfo.city:1:5);
                   x8cadr = %trim(ExtrAlphaNum(fmnm01)) + ' ' +
                   %trim(ExtrAlphaNum(fmnm02));
                Else;
                   x8cadr = %trim(ExtrAlphaNum(fmnm01));
                Endif;

                x8ccity = %trim(ExtrAlphaNum(dsPeoInfo.city));
                x8cst   = dsPeoInfo.state;
                x8czip  = dsPeoInfo.zip;
                //*** Beg Change *** 08/01/17 *********************************
                //x8cfein = %subst(fmc101:1:9);
                x8cfein = %subst(fmc101:2:9);
                //*** End Change *** 08/01/17 *********************************
                PEData = %trim(e48315);

                PEName = %trim(ExtrAlphaNum(PEName));
                Update ppe01;
             Endif;
          Endif;
           //*** End   add ***  04/21/17  ****************************************
      /End-Free
     c                   endsr
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
      *
     C                   time                    dsdttm
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         sysdat            7 0
      *
     c                   exsr      getfnd
    
     c                   endsr
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
    
     C     emky2         chain     pem01
     C                   if        (%found) and (emlsid = '5')
     C                   move      *on           crtfrm
     C                   endif                                                  emp_ls = '5'
    
     C                   endsr
