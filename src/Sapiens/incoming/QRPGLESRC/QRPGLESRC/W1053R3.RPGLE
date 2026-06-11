     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')

     H/TITLE W1053R3 - Endorsement Re-Attach
      *====================================================================
      * Program Description:  This program performs processing similar to
      *  what takes place during an original bill.  It will delete any
      *  auto attached endorsements and reprocess them.
      *
      * Called From: W1053R
      *
      * Calls To:    None
      *
      * Created:     05/30/2000
      * Programmer:  C:113
      * Project:     CM027
      *====================================================================
      * Revisions:
      * 06/14/21 C:180 Project: WC2795
      *                Call SRPEO with endPlist1 Parameter list.
      *
      * 01/19/21 C:180 Project: WC2786
      *                Add unexpired date to attachment criteria.
      *
      * 11/16/20 C:180 Project: WC2775
      *                Call SRDEDUCT with endPlist1 Parameter list.
      *
      * 05/14/20 C:291 Project: WC2608
      *                Call SROFFICER with endPlist1 Parameter list.
      *
      * 02/27/20 C:136 Project: WC2549
      *                Call SRPEOCLT with endPlist1 Parameter list.
      *
      * 06/26/19 C:180 Project: WC2677
      *                Call SRNOE with endPlist1 Parameter list.
      *
      * 10/19/18 C:291 Project: WC2549
      *                Call SRALTEMP with endPlist1 Parameter list.
      *
      * 07/19/18 C:291 Project: WC2627
      *                Call SRCONTCLS with endPlist1 Parameter list.
      *
      * 04/12/18 C:062 Project: 150WCX
      *                Call SRLRGDED with endPlist1 Parameter list.
      *
      * 09/06/17 C:291 Project: 150WBM
      *                Call SRNOTICE with endPlist1 Parameter list.
      *
      * 03/13/09 C:61  SUPPORT WA093  67589
      *                Check for Employer History (WDEHP) record to
      *                determine if bill has been run.
      *
      * 09/18/08 C:61  PROJECT WC1744 PRISS 21
      *                Only consider endorsement for attachment if
      *                Edition Date greater than or equal policy effective
      *                date of state reporting division.
      *
      * 11/01/07 C:184 PROJECT WC1530
      *                Comments in code for changes made 03/28/07
      *                (WC1518) need to be changed to reference WC1530
      *
      * 07/17/07 C:184 PROJECT WC1529
      *                Only attach endorsement if endorsement for is
      *                authorized for client.
      *
      * 04/12/07 C:184 PROJECT WC1528
      *                 Add additional parameter FORM_NAME to call SRTEXT pgm.
      *
      * 03/28/07 C:184 PROJECT WC1518
      *                Endorsement "Approved by State" Edit
      *
      * 02/08/05  C:61  SUPPORT WA139 51327
      *                 Change to not process 'E'xcluded employer attached
      *                 endorsement records.
      *
      * 07/20/05  C:61  SUPPORT WA129 48000
      *                 Changed to remove endorsements without respect to
      *                 division.
      *
      * 12/15/03  C:09  PROJECT CM027
      *                 On ErrorChk subroutine only check for estimated
      *                 payroll to be zero, not both S/R and estimated
      *
      *
      * ------------------------------------------------------------------
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.
      * ------------------------------------------------------------------
      *====================================================================

      *--------------------------------------------------------------------
      * File Specifications
      *--------------------------------------------------------------------
     FWDELP     if   e             disk
     FWMEMP     if   e             disk
     FWDEHL     if   e           k disk
     FWMMUL6    uf   e           k disk
     FWMMUL5    if   e           k disk
     F                                     rename(pmu01:pmu05)
     FWTENL2    if   e           k disk
      *--------------------------------------------------------------------
      * Prototypes
      *--------------------------------------------------------------------
     D ProcEnd         PR

     D ErrorChk        PR             1a
     D  company                       3  0
     D  group                         3  0
     D  employer                      9  0
     D  division                      5  0
     D  polYear                       3  0

      /copy WPREMPLR

      *--------------------------------------------------------------------
      * Stand-alone fields
      *--------------------------------------------------------------------
     D company         s              3  0
     D group           s              3  0
     D employer        s              9  0
     D division        s              5  0
     D polYear         s              3  0
     D eCompany        s              3a
     D eGroup          s              3a
     D eEmployer       s              9a
     D ePolYear        s              3a
     D eRtnCode        s              1a
     D empRRN          s              9  0
     D divRRN          s                   like(empRRN)
     D endorse#        s                   like(muend#)
      *--------------------------------------------------------------------
      * PLIST definitions
      *--------------------------------------------------------------------
      ** Entry parameters
     C     *entry        plist
     C                   parm                    eCompany
     C                   parm                    eGroup
     C                   parm                    eEmployer
     C                   parm                    ePolYear
     C                   parm                    eRtnCode
      *====================================================================
      * Main
      *====================================================================
     ** Check to see if the employer has been billed
     C                   eval      eRtnCode = ErrorChk(company:
     C                                                 group:
     C                                                 employer:
     C                                                 division:
     C                                                 polYear)

      ** If he has, then process the endorsements, else just leave with the
      ** return code.
     C                   if        eRtnCode = '1'
     C                   callp     ProcEnd
     C                   endif

     C                   move      *on           *inlr
      *====================================================================
      * *inzsr - Program Initialization Subroutine
      *====================================================================
     C     *inzsr        begsr

     C                   move      eCompany      company
     C                   move      eGroup        group
     C                   move      eEmployer     employer
     C                   move      ePolYear      polYear
     C                   eval      endorse# = 0

      ** Find the state reporting division for this employer
     C                   callp     GetStRptDv(company:group:employer:empRRN)
     C
     C     empRRN        chain     pem01                              60
     C                   eval      division = EMDIV

      ** Get employer detail record for the state reporting division
     C                   eval      divRRN = GetEmpYrRR(polYear:employer: +
     C                                                 division:group:company)
     C     divRRN        chain     pel01                              60

     C                   endsr                                                  *inzsr

      *====================================================================
      * Reattach - Process endorsement attachment programs
      *====================================================================
     P ProcEnd         B
     D ProcEnd         PI

     D pCompany        s                   like(eCompany)
     D pGroup          s                   like(eGroup)
     D pEmployer       s                   like(eEmployer)
     D pDivision       s              5a
     D pPolYear        s                   like(ePolYear)
     D pMode           s              1a   inz('1')

     C     endPlist      plist
     C                   parm                    pCompany
     C                   parm                    pGroup
     C                   parm                    pEmployer
     C                   parm                    pDivision
     C                   parm                    pPolYear
     C                   parm                    pMode

     C     endPlist1     plist
     C                   parm                    pCompany
     C                   parm                    pGroup
     C                   parm                    pEmployer
     C                   parm                    pDivision
     C                   parm                    pPolYear
     C                   parm                    pMode
     C                   parm      enform        pform            15

     C     enKey         klist
     C                   kfld                    company
     C                   kfld                    group

     C     muKey         klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    polYear
     C                   kfld                    employer

     C     muKy5         klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    polYear
     C                   kfld                    employer
     C                   kfld                    enform
     C                   kfld                    endorse#

      ** Delete any previously auto-attached original bill endorsements
     C     muKey         chain     pmu01                              60
     C                   dow       NOT *in60
     C                   if        MUEND# = *zero and
     C                             MUMNAU = 'A'
     C                             and MUEXFL <> 'E'
     c                             and mupgm <> 'W7413R'
     C                   delete    pmu01
     C                   endif
     C     muKey         reade     pmu01                                  60
     C                   enddo

      ** Process thru the endorsement form table file
     C     enKey         chain     pen01                              60
     C                   dow       NOT *in60

      *** Begin Change *** 01/19/21 ****************************************
     C***                if        ENEDDT <= ELPFDT
       If (ENedDt <= ELpFdt and ENunExpDt = *Zero) or
          (ENunExpDt > *Zero and ENunExpDt >= ELpFdt and ELpTdt > ENunExpDt);
      *** End Change   *** 01/19/21 ****************************************
     C*Valid Endorsement State
     C
     C                   call      'SRVAES'
     C                   parm      enform        ##form           15
     C                   parm      *blank        ##rtn             1
     C*
     C                   if        ##rtn = 'Y'

     C                   if        ENMNAU = 'A' and
     C                             ENPGM1 <> *blank and
     C                             ENSTAT <> 'I'

     C                   if        ENRPDT = *zeros or
     C                             ENRPDT > ELPFDT

     c     muKy5         chain     pmu05
     c                   if        not (%found)
     C                   move      company       pCompany
     C                   move      group         pGroup
     C                   move      employer      pEmployer
     C                   move      division      pDivision
     C                   move      polYear       pPolYear
     c
     c                   Call      'SRCKES'
     c                   Parm                    company
     c                   Parm                    group
     c                   Parm                    employer
     c                   Parm                    polYear
     c                   Parm                    enform
     c                   parm                    ##attch           1
     c
      *** beg chg ***** 09/06/17 ***************************
     c***                   If        ##attch = 'Y'
     C***                   If        ENPGM1 = 'SRTEXT'
     C***                   call      'SRTEXT'      endPlist1
     C***                   else
     C***                   call      ENPGM1        endPlist
     C***                   endif
     c***                   endif
      /Free
       Select;
       When ##attch <> 'Y';
       //*** Begin Chg *** 04/12/18 ***********************
       //********** When ENpgm1 = 'SRTEXT' Or
       //**********  ENpgm1 = 'SRNOTICE';
       //*** Begin Chg *** 07/19/18 ***********************
       //******When ENpgm1 = 'SRTEXT' Or ENpgm1 = 'SRNOTICE' Or
       //******   ENpgm1 = 'SRLRGDED';
       //*** Begin Chg *** 10/19/18 ***********************
       //******When ENpgm1 = 'SRTEXT' Or ENpgm1 = 'SRNOTICE' Or
       //******   ENpgm1 = 'SRLRGDED' Or ENpgm1 = 'SRCONTCLS';
       //**** Begin Chg *** 06/26/19 *************************
       // When ENpgm1 = 'SRTEXT' Or ENpgm1 = 'SRNOTICE' Or
       //    ENpgm1 = 'SRLRGDED' Or ENpgm1 = 'SRCONTCLS' Or
       //    ENpgm1 = 'SRALTEMP';
       //*** Beg Chg ***** 02/27/20 *************************
       //*** When ENpgm1 = 'SRTEXT' Or ENpgm1 = 'SRNOTICE' Or
       //***  ENpgm1 = 'SRLRGDED' Or ENpgm1 = 'SRCONTCLS' Or
       //***  ENpgm1 = 'SRALTEMP' or ENpgm1 = 'SRNOE';
       When ENpgm1 = 'SRTEXT'
        Or ENpgm1 = 'SRNOTICE'
        Or ENpgm1 = 'SRLRGDED'
        Or ENpgm1 = 'SRCONTCLS'
        Or ENpgm1 = 'SRALTEMP'
        Or ENpgm1 = 'SRNOE'
        Or ENpgm1 = 'SRPEOCLT'
        //*** Beg Add ***** 05/14/20 **************************
        Or ENpgm1 = 'SROFFICER'
        //*** End Add ***** 05/14/20 **************************
        //*** Beg Add ***** 11/16/20 **************************
        Or ENpgm1 = 'SRDEDUCT'
        //*** End Add ***** 11/16/20 **************************
        //*** Beg Add ***** 06/14/21 **************************
        Or ENpgm1 = 'SRPEO'
        //*** End Add ***** 06/14/21 **************************
        ;
       //*** Beg Chg ***** 02/27/20 *************************
       //**** End   Chg *** 06/26/19 *************************
       //*** End   Chg *** 10/19/18 ***********************
       //*** End   Chg *** 07/19/18 ***********************
       //*** End   Chg *** 04/12/18 ***********************
          Exsr CallWPlist1;
       Other;
          Exsr CallWPlist;
       Endsl;
      /End-Free
      *** end chg ***** 09/06/17 ***************************

     C                   endif
     C                   endif
     C                   endif
     C                   endif
     C                   endif

     C     enKey         reade     pen01                                  60
     C                   enddo

      *-----------------------------------------------------
     c     CallWPlist    Begsr
      *-----------------------------------------------------
     c                   Call      ENpgm1        EndPlist
     c                   Endsr
      *-----------------------------------------------------
     c     CallWPlist1   Begsr
      *-----------------------------------------------------
     c                   Call      ENpgm1        EndPlist1
     c                   Endsr

     P ProcEnd         E
      *====================================================================
      * ErrorChk - Check for errors
      *====================================================================
     P ErrorChk        B

     D ErrorChk        PI             1a
     D  company                       3  0
     D  group                         3  0
     D  employer                      9  0
     D  division                      5  0
     D  polYear                       3  0

     C     ehKey         klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    employer
     C                   kfld                    division
     C                   kfld                    polYear

      ** Do not process if employer has not been billed.
      ** If Employer History record exists then employer has been billed.

     c     ehKey         chain     peh01
      *
     c                   if        %found
     c                   return    '1'
     c                   else
     c                   return    '0'
     c                   endif


     P ErrorChk        E

