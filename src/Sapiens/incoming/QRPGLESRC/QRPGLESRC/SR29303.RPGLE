     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
    H/TITLE SR29303 - NJ - Employee Leasing Labor Contractor Endorsement
     *===============================================================*
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 12/08/05  C:61  Project: 139WBU                   *
     *===============================================================*
     * REVISIONS:
      *                                                               *
      * 11/03/22 C:333 Project: WC2834                                *
      *                Endorsement Recognition of Cancellation        *
      *
      * 03/05/19 C:180 Support 74777
      *                Additional changes for
     *                PEO attachment criteria moved to copy code
      *                Ref 10/06/17.
      *
      * 02/21/19 C:180 Support: 74777
      *                Report only once: The state reporting division.
      *
      * 01/02/19 C:180 Support 75920
      *                Fix removal of work file records.
      *
      * 12/26/18 C:180 Support: 74777
      *                Fix duplicate record writing.
      *                No pgm changes - Changed force ration in WXLLP.
      *                Recompiled only.
      *
     * 10/06/17 C:136 Project: 150WBL
     *                PEO attachment criteria moved to copy code
     *
      * 07/25/17 C:180 Project: 150WAP                                *
      *                Handle groups with combined PEOs.              *
      *                                                               *
      * 03/28/17 C:180 Project: WC2481
      *                - Remove punctuation issues for WCPOLS
      *
     * 03/02/17 C:136 Project: WC2481
     *                Fix auto attach and only print div info
     *
     * 09/29/16 C:33  Project: SY371
      *               -Implement Scriptura.
      *               -No comments.
      *
      * 08/22/11 C:061 support wa139 77394
      *                Do not include "DBA" in name if Federal Name
      *                has not been entered.
      *
      * 04/13/09 C:190 Project: 114WTU
      *                Changed endorsement processing program to load *
      *                merge-data file fields so as to make room for  *
      *                the expanded name.                             *
      *
      * 05/10/07 C:61  Project: 114WGH
      *                RE-write.  Change to print one copy of
      *                endorsement for each unique name and fein.
      *                Names and FEINs: Employer divisions;
      *                Addtitional Named Insureds; Entity/Locations.
      *
      * 11/27/06 C:15  Project: 139WFS                                *
      *                Added code to process the new GIC PEO type 'G' *
      *                the same as PEO type 'M'.                      *
     *                                                               *
     *  08/25/06 C:65  PROJECT: 114WGH                               *
     *           C:61- Add processing to write endorsement data to   *
     *                 the WXPEP reporting file for PA/DE/NJ states. *
     *               - Allow policy # to default.
     *
     * 04/28/06 C:61  support cm027
     *                Attach to policies active in the future.
      *
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
     * Copybook F specs
     F/COPY SCOPYEN01F
    
      ** Statistical Reporting/Rating File
     Fwdrrl     if   e           k disk
    
      ** Work File WC290303
     Fwxlll     uf   e           k disk
     Fwxllp     o    e             disk
     F                                     rename(pll01:pll02)
    
      ** Additional Named Insured
     Fwmanl     if   e           k disk
    
      ** Entity / Location
     Fwmall     if   e           k disk

      * WCPOLS Endorsement transaction
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     d nameds          ds
     d  llnm01
     d  llnm02
     d  llnm03
     d  llnm04
     d  llnm05
     d  llnm06
    d name                                OVERLAY(nameds)
     d                                     LIKE(llnm01) DIM(6)

     d EFDS            ds
     d  EMEFD1
     d  EMEFD2
     d  EMEFD3
     d  EMEFD4
     d  EMEFD5
    d EF                                  OVERLAY(EFDS)
     d                                     LIKE(EMEFD1) DIM(5)

     d CDDS            ds
     d  EMCAD1
     d  EMCAD2
     d  EMCAD3
     d  EMCAD4
     d  EMCAD5
    d CD                                  OVERLAY(CDDS)
     d                                     LIKE(EMCAD1) DIM(5)

     * Copybook D specs
     D/COPY SCOPYEN01D
     D/COPY WWCPOLDAT4
     D/COPY SPRSTRING

     *=========================================================================
     *  /COPY required constants
     *=========================================================================

     * Form ID
     D qqForm          s             15a   inz('WC290303  ')

     * Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')

     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)
     d x1              s              3  0
     d xxcxdt          s                   like(emcad1)
     d kkdttm          s                   like(lldttm)
     d kkname          s                   like(llname)
     d kktxid          s                   like(lltxid)

      *entry parameters
     D SR29303         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR29303         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
    
     ** adjust line
     c     linepl        pList
     c                   parm                    #1line          256
     c                   parm      46            #1length          3 0
     c                   parm      *blanks       #1remain        256
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================

     ** Entity / Location

     c     alky          klist
     c                   kfld                    emco#
     c                   kfld                    emfnd
     c                   kfld                    ememp#
     c                   kfld                    emdiv

     ** Addtional named Insured

     c     anky          klist
     c                   kfld                    emco#
     c                   kfld                    emfnd
     c                   kfld                    ememp#
     c                   kfld                    emdiv

     ** WC290303 Work File

     c     llky          klist
     c                   kfld                    kkdttm

     c     llky1         klist
     c                   kfld                    kkdttm
     c                   kfld                    kkname
     c                   kfld                    kktxid

     ** Employer

     c     emky1         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv

     ** Reporting Rating

     c     rrky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    svjrst
     c                   kfld                    elpfdt
      *
     c     rrky1         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    svjrst

      **

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
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     * Copybook c specs
     c/COPY SCOPYEN01C
    
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     c     othinf        begsr

     c     elky2         setll     pel02
     c     elky2         reade     pel02

     c                   dow       (not %eof)

       //***** Beg Add *** 11/03/22 ******************************
          xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
       //***** End Add *** 11/03/22 ******************************
     C                   Clear                   RSjrSt
     C     rsky          Chain     prs01                              20
     C                   If        RSjrSt = ENst

     c     emky1         chain     pem01

      *** Begin Delete *** 03/05/19 ****************************************
      *  If not PEO master division then write employer name
      *** Begin change *** 07/25/17 *********************************************
     c****               if        FNpeof = 'M'
     c*****  03/05/19    if        FNpeof = 'C' and ELemp# = 1 and eldiv = 0
      **  skip
     c*****  03/05/19    else
     c*****  03/05/19
     c*****  03/05/19    if        (FNpeof = 'M') or
     c*****  03/05/19              (FNpeof = 'C' and ELemp# <> 1)
      *** End   change *** 07/25/17 *********************************************
      *** End   Delete *** 03/05/19 ****************************************

      *    Get cancel date
     c                   eval      x1 = 5

     c                   dow       (ef(x1) = 0)
     c                             and (x1 > 1)
     c                   eval      x1 = x1 - 1
     c                   enddo

     c                   eval      xxcxdt = cd(x1)


      *    If not cancelled or cancellation date is greater than
      *     policy effective date.

     c                   if        (xxcxdt = 0) or (elpfdt < xxcxdt)

      *      Write Employer division name

     c                   exsr      wrtdvn
      /Free
       //*** Begin Add *** 02/21/19 ***************************************
       // Only the state reporting division needs processing.
                         Leave;
       //*** End Add *** 02/21/19 ***************************************
      /End-Free

      *      Write Additional Named Insured names

     c                   exsr      wrtadn

      *      Write Entity/Location names

     c                   exsr      wrteln
     c                   endif

      *** Begin Delete *** 03/05/19 ****************************************
     c*** 03/05/19       endif
      ***
      *** Begin Add *** 07/25/17 *********************************************
     c*** 03/05/19       endif
      *** End   Add *** 07/25/17 *********************************************
      *** End   Delete *** 03/05/19 ****************************************
     c                   endif

          //***** Beg Add *** 11/03/22 ******************************
          Endif;
          //***** End Add *** 11/03/22 ******************************
     c     elky2         reade     pel02
     c                   enddo
    
      *  Write to endorsement form work file WXFMP

     c                   exsr      wrtWXFMP
     c                   endsr
     *=========================================================================
     * WRTDVN - Prepare Employer Division Names for printing
     *=========================================================================
     c     wrtdvn        begsr

     c                   clear                   pll02
     c                   eval      lldttm = kkdttm

      * employer tax ID

     c                   if        cltid = '114'
     c                   eval      lltxid = emue#
     c                   else
     c                   eval      lltxid = emsemp
     c                   endif

      * employer name

     c                   if        (emfdnm <> *blanks)
     c                             or (emfdn2 <> *blanks)
     c                   eval      llname = %trim(emfdnm)
     c                                    + %trim(emfdn2)
     c                                    + ' DBA '
     c                                    + %trim(emname)
     c                                    + %trim(emnam2)

     c                   else

     c                   eval      llname = %trim(emname)
     c                                    + %trim(emnam2)
     c                   endif

     c                   exsr      prepare_print

      * employer physical address

     c                   if        empad1 <> *blanks
     c                   eval      lladr1 = empad1
     c                   eval      lladr2 = empad2
     c                   eval      lladr3 = %trim(empcty)
     c                                    + ', '
     c                                    + %trim(empst)
     c                   move      empzip        xxzip
     c                   cat       xxzip1:1      lladr3
    
     c                   if        xxzip2 <> *blank
     c                             and xxzip2 <> *zero
     c                   cat       '-':0         lladr3
     c                   cat       xxzip2:0      lladr3
     c                   endif

     c                   eval      llad60 = %trim(lladr1)
     c                                    + ', '
     c                                    + %trim(lladr3)

     c                   if        (lladr2 = *blanks)
     c                   eval      lladr2 = lladr3
     c                   eval      lladr3 = *blanks
     c                   endif

     c                   else

      * employer mailing address

     c                   eval      lladr1 = emmad1
     c                   eval      lladr2 = emmad2
     c                   eval      lladr3 = %trim(emmcty)
     c                                    + ', '
     c                                    + %trim(emmst)
     c                   move      emmzip        xxzip
     c                   cat       xxzip1:1      lladr3
    
     c                   if        xxzip2 <> *blank
     c                             and xxzip2 <> *zero
     c                   cat       '-':0         lladr3
     c                   cat       xxzip2:0      lladr3
     c                   endif

     c                   eval      llad60 = %trim(lladr1)
     c                                    + ', '
     c                                    + %trim(lladr3)

     c                   if        (lladr2 = *blanks)
     c                   eval      lladr2 = lladr3
     c                   eval      lladr3 = *blanks
     c                   endif

     c                   endif

     c                   eval      llusr = q1user
     c                   eval      llpgm = q1pgm
     c                   time                    dsdttm
     c                   eval      lltim = dstime
                                                                                         : :
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         lldat

     c                   eval      kkname = llname
     c                   eval      kktxid = lltxid
     c     llky1         chain(n)  pll01

     c                   if        not %found
     c                   write     pll02
     c                   endif

     c                   endsr
     *=========================================================================
     * PREPARE_PRINT - Prepare Name and Tax ID for printing
     *=========================================================================
     c     prepare_print begsr

      * Prepare Name to print

     c                   eval      llname = %trim(llname)
     c                   clear                   name
     c                   eval      x1 = 1
     c                   eval      #1line = llname

     c                   dou       (x1 = 6) or (#1remain = *blanks)
     c                   Call      'SRLINE'      linepl
     c                   eval      name(x1) = #1line
     c                   eval      #1line = #1remain
     c                   eval      x1 = x1 + 1
     c                   enddo

      * prepare Tax ID to print

     c                   eval      x1 = 1

     c                   dow       (x1 < 7) and (lltxid <> *blanks)

     c                   if        %subst(name(x1):1:7) = 'Tax ID:'
     c                   leave
     c                   endif

     c                   if        name(x1) = *blanks
     c                   eval      name(x1) = 'Tax ID: ' + %trim(lltxid)
     c                   leave
     c                   endif

     c                   eval      x1 = x1 + 1
     c                   enddo

     c                   endsr
     *=========================================================================
     * WRTADN - Prepare Additional Named Insured for printing
     *=========================================================================
     c     wrtadn        begsr

     c     anky          setll     pan01
     c     anky          reade     pan01

     c                   dow       not %eof

      *  Additional Named Insured

     c                   clear                   llname
     c                   eval      llname = %trim(anname)
     c                                    + %trim(annam2)

     c                   exsr      prepare_print

     c                   time                    dsdttm
     c                   eval      lltim = dstime
                                                                                         : :
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         lldat

     c                   eval      kkname = llname
     c                   eval      kktxid = lltxid
     c     llky1         chain(n)  pll01

     c                   if        not %found
     c                   write     pll02
     c                   endif
     c     anky          reade     pan01
     c                   enddo

     c                   endsr


     *=========================================================================
     * wrteln - Prepare Entity/Location Names for printing
     *=========================================================================
     c     wrteln        begsr

     c     alky          setll     pal01
     c     alky          reade     pal01

     c                   dow       not %eof
     c                   clear                   pll02
     c                   eval      lldttm = kkdttm

      * Entity/Location tax ID

     c                   if        cltid = '114'
     c                   eval      lltxid = alue#
     c                   else
     c                   eval      lltxid = alsemp
     c                   endif

      * Entity/Location name

     c                   if        (alfdnm <> *blanks)
     c                             or (alfdn2 <> *blanks)
     c                   eval      llname = %trim(alfdnm)
     c                                    + %trim(alfdn2)
     c                                    + ' DBA '
     c                                    + %trim(alname)
     c                                    + %trim(alnam2)

     c                   else

     c                   eval      llname = %trim(alname)
     c                                    + %trim(alnam2)
     c                   endif

     c                   exsr      prepare_print

      * entity / location physical address

     c                   if        alpad1 <> *blanks
     c                   eval      lladr1 = alpad1
     c                   eval      lladr2 = alpad2
     c                   eval      lladr3 = %trim(alpcty)
     c                                    + ', '
     c                                    + %trim(alpst)
     c                   move      alpzip        xxzip
     c                   cat       xxzip1:1      lladr3
    
     c                   if        xxzip2 <> *blank
     c                             and xxzip2 <> *zero
     c                   cat       '-':0         lladr3
     c                   cat       xxzip2:0      lladr3
     c                   endif

     c                   eval      llad60 = %trim(lladr1)
     c                                    + ', '
     c                                    + %trim(lladr3)

     c                   if        (lladr2 = *blanks)
     c                   eval      lladr2 = lladr3
     c                   eval      lladr3 = *blanks
     c                   endif

     c                   else

      * entity / location mailing address

     c                   eval      lladr1 = almad1
     c                   eval      lladr2 = almad2
     c                   eval      lladr3 = %trim(almcty)
     c                                    + ', '
     c                                    + %trim(almst)
     c                   move      almzip        xxzip
     c                   cat       xxzip1:1      lladr3
    
     c                   if        xxzip2 <> *blank
     c                             and xxzip2 <> *zero
     c                   cat       '-':0         lladr3
     c                   cat       xxzip2:0      lladr3
     c                   endif

     c                   eval      llad60 = %trim(lladr1)
     c                                    + ', '
     c                                    + %trim(lladr3)

     c                   if        (lladr2 = *blanks)
     c                   eval      lladr2 = lladr3
     c                   eval      lladr3 = *blanks
     c                   endif

     c                   endif

     c                   time                    dsdttm
     c                   eval      lltim = dstime
                                                                                         : :
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         lldat

     c                   eval      kkname = llname
     c                   eval      kktxid = lltxid
     c     llky1         chain(n)  pll01

     c                   if        not %found
     c                   write     pll02
     c                   endif

     c     alky          reade     pal01
     c                   enddo

     c                   endsr


     *=========================================================================
     * WRTWXFMP - Write records to Endorsement Form Work File
     *=========================================================================
     c     wrtWXFMP      begsr
      /Free
       WriteInCopybook = *Off;
      /End-Free

     c     llky          setll     pll01
     c     llky          reade     pll01

     c                   dow       not %eof
     c                   clear                   fmnm01
     c                   clear                   fmnm02
     c                   clear                   fmnm03
     c                   clear                   fmnm04
     c                   clear                   fmnm05
     c                   clear                   fmnm06
     c                   clear                   fmc401
     c                   clear                   fmc402
     c                   clear                   fmc403

     c                   eval      fmnm01 = llnm01
     c                   eval      fmnm02 = llnm02
     c                   eval      fmnm03 = llnm03
     c                   eval      fmnm04 = llnm04
     c                   eval      fmnm05 = llnm05
     c                   eval      fmnm06 = llnm06
     c                   eval      fmc401 = lladr1
     c                   eval      fmc402 = lladr2
     c                   eval      fmc403 = lladr3

      /Free
       //* Write the output record
       Exsr WriteWorkFile;
      /End-Free
     c                   exsr      endrpt

     c     llky          reade     pll01
     c                   enddo

     c                   endsr


     *=========================================================================
     * ENDRPT - Update Endorsement Reporting File
     *=========================================================================
     c     endrpt        begsr

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
     c                   clear                   E29303

      *** Begin Change 03/28/17 ***********************************
     c***                   eval      xgnm1 = %trim(llname)
     c***                   eval      xgad1 = %trim(llad60)
      /Free
        xgnm1 = %trim(ExtrAlphaNum(llname));
        xgad1 = %trim(ExtrAlphaNum(llad60));
      /End-Free
      *** End Change 03/28/17 ***********************************
     c                   move      *blanks       pedata
     c                   movel     E29303        pedata
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt
      *
     c                   if        xx1st = 'Y'
     C                   update    ppe01
     c                   eval      xx1st = 'N'

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

     c                   endsr

    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     c     pgmstr        begsr
    
     c                   move      'Y'           xx1st             1
     c                   exsr      getfnd
     c                   time                    dsdttm
     c                   eval      kkdttm = dsdttm
    
     c                   endsr
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     c     pgmend        begsr
     *  Delete work file records
      *
      **** Begin Change *** 01/02/19 **************************************
     c***  llky          delete    pll01
      /Free
       Dou not %found(WXLLL);
          Delete llky PLL01;
       Enddo;
      /End-Free
      **** End   Change *** 01/02/19 **************************************
     c                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
      /Free
       //MULTIPLE AUTO for PEO Group with NJ coverage
       Begsr Valid;
       crtFrm = *On;
       Endsr;
      /End-Free
